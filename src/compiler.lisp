(declaim (special *debug*))

(defun debug-format (&rest args)
	(when *debug*
		(apply #'format (cons t args))))

(defun everything (func tree)
  (funcall func (if (listp tree)
                    (mapcar (lambda (node) (everything func node)) tree)
                    tree)))

;; Top-down version
(defun everything-1 (func tree)
  (multiple-value-bind (new-tree skip) (funcall func tree)
    (if skip
        new-tree
        (if (listp new-tree)
            (mapcar (lambda (node) (everything-1 func node)) new-tree)
            new-tree))))

(defun everywhere (func tree)
  (when (listp tree)
    (mapc (lambda (node) (everywhere func node))
          tree))
  (funcall func tree))

;; Top-down version
(defun everywhere-1 (func tree)
  (funcall func tree)
  (when (listp tree)
    (mapc (lambda (node) (everywhere-1 func node))
          tree)))

(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *lmacros* (make-hash-table :test #'eq)))

(defmacro deflmacro (name params &body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setf (gethash ',name *lmacros*)
           (lambda ,params ,@body))))

(defun macro-expand-lambda (term)
  (labels ((expand-node (node)
             (cond ((and (listp node)
                         (gethash (first node) *lmacros*))
                    (let ((new-node (apply (gethash (first node) *lmacros*) (cdr node))))
                      (expand-node new-node)))
                   (t node))))
    (everything-1 #'expand-node term)))

(defun canon-lambda (term)
  (labels ((canon-node (node)
             (cond ((and (listp node)
                         (eq (first node) 'lambda)
                         (listp (second node))) ;;(lambda (vars) body) case
                    (let ((body (third node)))
                      (reduce (lambda (el body)
                                `(lambda ,el ,body))
                              (second node)
                              :initial-value body
                              :from-end t)))
                   ((and (listp node)
                         (not (eq (first node) 'set))
                         (not (eq (first node) 'lambda))
                         (>= (length node) 2)) ;;(a b c ..) case
                    (reduce (lambda (body el)
                              (list body el))
                            (cddr node)
                            :initial-value (list (first node) (second node))))
                   (t node))))
    (everything #'canon-node term)))

(defun occures-free (var term)
  (everywhere-1 (lambda (node)
                  (cond ((eq var node)
                         (return-from occures-free t))
                        ((and (listp node)
                              (eq (first node) 'lambda)
                              (eq (second node) var)) ;; (lambda var body) case
                         (return-from occures-free nil))))
                term)
  nil)

;;(defvar *combinator-form-dump* "/home/myth/projects/icfp2011/icfp-2011/comb-dump.txt")
;;(defvar *compiled-form-dump* "/home/myth/projects/icfp2011/icfp-2011/prog-dump.txt")
(defvar *combinator-form-dump* nil)
(defvar *compiled-form-dump* nil)

(defun compile-lambda-to-combinators (term)
  (labels ((compile-node (node)
             (let ((res (compile-node-1 node)))
               ;;(format t "~A -> ~A~%" node res)
               res))
           (compile-node-1 (node)
             (cond ((atom node)  ;; x
                    node)
                   ((and (listp node)
                         (eq (first node) 'set))  ;; (set slot e2)
                    (list 'set 
                          (second node)
                          (compile-lambda-to-combinators (third node))))
                   ((and (listp node)
                         (not (eq (first node) 'lambda)))  ;; (e1 e2)
                    (list (compile-lambda-to-combinators (first node))
                          (compile-lambda-to-combinators (second node))))
                   ((not (occures-free (second node) (third node)))  ;; (lambda x e), x does not occur free in e
                    (list 'k
                          (compile-lambda-to-combinators (third node))))
                   ((eq (second node) (third node))  ;; (lambda x x)
                    'i)
                   ((and (listp (third node))
                         (eq (first (third node)) 'lambda))  ;; (lambda x (lambda y e))
                    (compile-lambda-to-combinators `(lambda ,(second node) ,(compile-lambda-to-combinators (third node)))))
                   (t  ;; (lambda x (a b))
                    (list (list 
                           's 
                           (compile-lambda-to-combinators `(lambda ,(second node) ,(first (third node)))))
                          (compile-lambda-to-combinators `(lambda ,(second node) ,(second (third node)))))))))
    (let ((res (compile-node term)))
      (when *combinator-form-dump*
        (with-open-file (*standard-output*
                         *combinator-form-dump*
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
          (format t "~A" res)))
      res)))

(deflmacro progn (&rest body)
  (let ((syms (mapcar (lambda (x) (declare (ignore x)) (gensym))
                      body)))
    `((lambda ,syms ,(car (last syms))) ,@body)))

(deflmacro loop (var &rest body)
  `(S (lambda (,var) (progn ,@body (get ,var))) I))

(deflmacro + (var q-num)
  (compile-int (second q-num) var))

(deflmacro let (vars &rest body)
  (reduce 
   (lambda (v expr)
     `((lambda ,(first v) ,expr) ,(second v)))
   vars
   :from-end t
   :initial-value (cons 'progn body)))

(defun compile-int (num &optional (start 'zero))
  (cond ((= num 0)
         start)
        ((= (mod num 2) 0)
         (list 'dbl (compile-int (/ num 2))))
        (t (list 'succ (compile-int (1- num))))))

(defun compile-all-ints (term)
  (everything (lambda (num)
                (if (and (listp num)
                         (eq (first num) 'quote)
                         (integerp (second num)))
                    (compile-int (second num))
                    num))
              term))

(defun show-card (sym)
  (case sym
    ((i k s) (string-upcase (symbol-name sym)))
    (otherwise (string-downcase (symbol-name sym)))))

(defun show-cmd (cmd)
  (case cmd
    (left 1)
    (right 2)))

;; left - card to slot, 1
;; right - slot to card, 2

(defvar *current-program*)
(defvar *free-slots* nil)

(defun add-command (cmd)
  (push cmd *current-program*))

(defun end-program ()
  (setf *current-program* (nreverse *current-program*)))

(defun init-free-slots ()
  (setf *free-slots* (loop for i from 1 to 255 collect i)))

(defun alloc-slot ()
  (let ((res (pop *free-slots*)))
    (debug-format "Allocated slot ~A~%" res)
    res))

(defun alloc-specific-slot (slot)
  (debug-format "Allocated specific slot ~A~%" slot)
  (setf *free-slots* (remove slot *free-slots* :test #'=)))

(defun free-slot (slot)
  (debug-format "Freed slot ~A~%" slot)
  (push slot *free-slots*))

(defun compile-move (slot-from slot-to)
  (compile-combinators-to-program-1 (compile-int slot-from) slot-to)
  (add-command (list 'left 'get slot-to `(move ,slot-from ,slot-to))))

(defun compile-combinators-to-program-1 (term target-slot-num)
  (cond ((symbolp term)
         (add-command (list 'left 'put target-slot-num))
         (add-command (list 'right term target-slot-num term)))
        ((integerp term)
         )
        ((eq (first term) 'set) ;; (set slot e)
         ;;(add-command (list 'left 'put (second term)))
         (alloc-specific-slot (second term))
         (compile-combinators-to-program-1 (third term) (second term)))
        ;; ((and (atom (first term))
        ;;       (atom (second term))) ;; (card1 card2)
        ;;  (add-command (list 'left 'put target-slot-num))
        ;;  (add-command (list 'right (second term) target-slot-num))
        ;;  (add-command (list 'left (first term) target-slot-num)))
        ((symbolp (first term)) ;; (card E)
         (compile-combinators-to-program-1 (second term) target-slot-num)
         (add-command (list 'left (first term) target-slot-num term)))
        ((symbolp (second term)) ;; (E card)
         (compile-combinators-to-program-1 (first term) target-slot-num)
         (add-command (list 'right (second term) target-slot-num term)))
        ((= target-slot-num 0) ;; (E1 E2) in slot 0, which is a special case
         ;; because we need slot 0 for E2
         (let ((tmp-slot (alloc-slot)))
           (compile-combinators-to-program-1 term tmp-slot)
           (compile-move tmp-slot 0)
           (free-slot tmp-slot)))
        (t  ;; (E1 E2)
         (let ((tmp-slot (alloc-slot)))
           (compile-combinators-to-program-1 (first term) target-slot-num)
           (compile-move 0 tmp-slot)
           (compile-combinators-to-program-1 (second term) 0)
           (add-command `(left K ,target-slot-num))
           (add-command `(left S ,target-slot-num))
           (add-command `(right get ,target-slot-num))
           (add-command `(right zero ,target-slot-num ,term))
           (compile-move tmp-slot 0)
           (free-slot tmp-slot)))))

(defun compile-combinators-to-program (term &optional (target-slot-num nil))
  (let ((*current-program* nil))
    (unless target-slot-num
      (setf target-slot-num (alloc-slot)))
    (compile-combinators-to-program-1 term target-slot-num)
    (end-program)
    *current-program*))

(defun compile-lambda (term &key (target-slot 1) (prealloc-slots nil))
  (init-free-slots)
  (mapc #'alloc-specific-slot prealloc-slots)
  (let ((res (if (and (listp term)
                      (eq (first term) 'progn)) ;; Top level 'progn' optimization
                 (apply #'append
                        (mapcar (lambda (sub-term)
                                  (compile-lambda-1 sub-term :target-slot target-slot))
                                (cdr term)))
                 (compile-lambda-1 term :target-slot target-slot)))
        (cnt -1))
    (when *compiled-form-dump*
      (with-open-file (*standard-output*
                       *compiled-form-dump*
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
        (mapc (lambda (cmd)
                (case (first cmd)
                  (left (format t "~A: left ~A ~A ;; ~A~%" (incf cnt) (show-card (second cmd)) (third cmd) (if (fourth cmd) (fourth cmd) "" )))
                  (right (format t "~A: right ~A ~A ;; ~A~%" (incf cnt) (third cmd) (show-card (second cmd)) (if (fourth cmd) (fourth cmd) "" )))))
              res))
      res)))

(defun compile-lambda-1 (term &key (target-slot 1))
  (compile-combinators-to-program (compile-lambda-to-combinators (canon-lambda (compile-all-ints (macro-expand-lambda term)))) target-slot))

(defun print-command (cmd)
  (case (first cmd)
    (left (format t "1~%~A~%~A~%" (show-card (second cmd)) (third cmd)))
    (right (format t "2~%~A~%~A~%" (third cmd) (show-card (second cmd))))))

(defun print-program (program fname)
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :supersede)
    (mapc #'print-command
          program)))

;; Program that is able to run a command on two slots
;;(print-program (compile-lambda 
;; '(progn 
;;   (set 1 (loop zz (inc (K '10 zz)))) 
;;   (set 0 (get '1))
;;   (set 1 (loop zz (inc (K '11 zz))))
;;   (set 1 (1 zero)))
;; :target-slot 2 :prealloc-slots '(1)) "/home/myth/projects/icfp2011/icfp-2011/example.input")

;;(print-program (compile-lambda 
;; '(progn 
;;   (set 1 (loop zz (help zz zz 8192))) 
;;   (set 0 (get '1))
;;   (set 0 (0 zero)))
;; :target-slot 2 :prealloc-slots '(1)) "/home/myth/projects/icfp2011/icfp-2011/example.input")
