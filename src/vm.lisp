(defparameter *max-slots* 256)
(defvar *max-live*  65535)
(defvar *init-live* 10000)
(defvar *max-move-applications-number* 1000)
(defvar *current-move-applications-count* 0)
(defvar *zombies-turn* nil)
(defvar *cur-turn* 0)
(defvar *max-turns* 100000)

(defmacro with-applications-limit (&body body)
  `(let ((*current-move-applications-count* 0))
     ,@body))

(defstruct (combinator-spec (:conc-name cs-))
  (name          'noname  :type symbol)
  (eval-function nil      :type function)
  (params-number 0        :type (integer 0 100)))

(defstruct (combinator (:conc-name cb-)
                       (:print-function (lambda (self stream _)
                                          (declare (ignore _))
                                          (let ((spec (cb-spec self))
                                                (params (cb-params self)))
                                            (format stream "@~A~A"
                                                    (cs-name spec)
                                                    (if params
                                                        (append (reverse params)
                                                                ;;(loop for _ from 1 to (- (cs-params-number spec) (length params)) collect
                                                                ;;    "_")
                                                                )
                                                        ""))))))
  spec
  params)

(defun copy-combinator-with-params (combinator params)
  (make-combinator :spec (cb-spec combinator)
                   :params params))

                                        ; defines '@' to be used to create new combinator instance, the syntax is:
                                        ;   @<some combinator defined by 'defcombinator'>
(eval-when (:load-toplevel :compile-toplevel :execute)
  (set-macro-character #\@ (lambda (stream char)
                             (declare (ignore char))
                             (list 'make-combinator ':spec `(gethash ',(read stream t nil t) *combinators*)))))

                                        ; raised for 'errors' specified in the doc
(define-condition game-logic-error (error) ())

(defun $p (comb &rest params)
  (copy-combinator-with-params comb params))

(defmacro in-game-check (condition &optional comment &rest comment-params)
  `(unless ,condition
     (debug-format "BANG: in game check ~A failed~A" ',condition (if ,comment ": " ""))
     (when ,comment
       (debug-format ,comment ,@comment-params))
     (debug-format "~%")
     (error 'game-logic-error)))

(defun cb-call-no-param (combinator-or-num)
  (if (integerp combinator-or-num)
      combinator-or-num
      (let* ((spec                (cb-spec combinator-or-num))
             (required-params-num (cs-params-number spec)))
        (cond
          ((= 0 required-params-num)
           (funcall (cs-eval-function spec)))
          (t combinator-or-num)))))

(defun cb-call (combinator param)
  (in-game-check (typep combinator 'combinator) "in call to cb-call")
  (in-game-check (< (incf *current-move-applications-count*)
                    *max-move-applications-number*)
                 "in call to cb-call")
  ;; Aaaaa
  ;;(push (cb-call-no-param param) (cb-params combinator))
  (let* ((spec                (cb-spec combinator))
         (params              (cons (cb-call-no-param param) (cb-params combinator)))
         (required-params-num (cs-params-number spec))
         (actual-params-num   (length params))
         (result (cond
                   ((< actual-params-num required-params-num) 
                    (copy-combinator-with-params combinator params))
                   ((= actual-params-num required-params-num)
                    (apply (cs-eval-function spec) (reverse params)))
                   (t (error "cb-apply called with more parameters then expected, combinator: ~A" combinator)))))
                                        ;(format t "cba: ~A~%" combinator)
    result))

                                        ; combinator symbol -> combinator spec
(defparameter *combinators* (make-hash-table))

(defmacro defcombinator (name (&rest params) &body body)
  (when (< (length params) 0)
    (error "error while defining combinator: ~A, number of parameters should not be less then 0" name))
  (let ((funname (gensym)))
    `(progn
       (defvar ,funname (make-combinator-spec
                         :name ',name
                         :eval-function (lambda ,params ,@body)
                         :params-number ,(length params)))
       (setf (gethash ',name *combinators*) ,funname))))

(defcombinator I (x) x)

#|
(defcombinator test (x y z) (format t "test: ~A, ~A, ~A~%" x y z))

(format t "~S~%" *combinators*)

(let ((ci @test))
  (cb-apply ci 1)
  (format t "~A~%" ci)
  (cb-apply ci 2)
  (format t "~A~%" ci)
  (cb-apply ci 3)
  (format t "~A~%" ci))
|#

(defstruct (ltg-slot (:print-function (lambda (self stream _)
                                        (declare (ignore _))
                                        (format stream "{~A: ~A}" (slot-vitality self) (slot-field self))))
                     (:conc-name slot-))
  (field    @I         ) ;:type (or (integer 0 *max-live*) function))
  (vitality *init-live*)) ;:type (integer -1 *max-live*)))

(defun dead? (slot)
  (<= (slot-vitality slot) 0))

(defun alive? (slot) (not (dead? slot)))

(defun valid-slot-number? (val)
  (and (integerp      val)
       (<= 0          val)
       (> *max-slots* val)))

(defun fill-array (array func)
  (loop for i from 0 to (1- (length array)) do
       (setf (aref array i) (funcall func)))
  array)

(defstruct player
  (slots (fill-array (make-array *max-slots* :element-type 'ltg-slot) #'make-ltg-slot)))

(defparameter *proponent* (make-player))
(defparameter *opponent*  (make-player))

(defun get-slot (player slot-number)
  (in-game-check (valid-slot-number? slot-number) "in call to get-slot, slot-number is ~A" slot-number)
  ;;(format t "gs: slots are: ~A~%" (player-slots player))
  (aref (player-slots player) slot-number))

(defmacro with-slot ((name player number) &body body)
  `(progn
     ;;(format t "ws: slot number ~A~%" ,number)
     (let ((,name (get-slot ,player ,number)))
       ;;(format t "ws: got slot: ~A~%" ,name)
       ,@body)))

(defmacro with-alive-slot ((name player number) &body body)
  `(with-slot (,name ,player ,number)
     (in-game-check (alive? ,name) "in call to with-alive-slot")
     ,@body))

(defcombinator zero () 0)

(defcombinator succ (n)
  (in-game-check (integerp n) "in call to succ, n is ~A" n)
  (if (< n *max-live*)
      (1+ n)
      *max-live*))

(defcombinator dbl (n)
  (in-game-check (integerp n) "in call to dbl, n is ~A" n)
  (if (< (* 2 n) *max-live*)
      (* 2 n)
      *max-live*))

(defcombinator get (i)
  (with-alive-slot (slot *proponent* i)
    (slot-field slot)))

(defcombinator put (unused) (declare (ignore unused)) @I)

(defcombinator S (f g x)
  (let* ((h (cb-call f x))
         (y (cb-call g x))
         (z (cb-call h y)))
    z))

(defcombinator K (x y) (declare (ignore y)) x)

(defcombinator inc (index)
  (with-slot (slot *proponent* index)
    (if (not *zombies-turn*)
                                        ; normal operation
        (when (and (< (slot-vitality slot) *max-live*)
                   (> (slot-vitality slot) 0))
          (incf (slot-vitality slot)))
                                        ; zombie operation
        (when (> (slot-vitality slot) 0)
          (decf (slot-vitality slot)))))
  @I)

(defun wrap-slot (number)
  (- (1- *max-slots*) number))

(defcombinator dec (i)
  (with-slot (slot *opponent* (wrap-slot i))
    (if (not *zombies-turn*)
                                        ; normal operation
        (when (> (slot-vitality slot) 0)
          (decf (slot-vitality slot)))
                                        ; zombie operation
        (when (and (< (slot-vitality slot) *max-live*)
                   (> (slot-vitality slot) 0))
          (incf (slot-vitality slot)))))
  @I)

(defcombinator attack (i j n)
  (with-slot (my-slot *proponent* i)
    (let ((v (slot-vitality my-slot)))
      (in-game-check (and (integerp n) (<= n v)) "in call to attack, n is ~A, v is ~A" n v)
      (setf (slot-vitality my-slot) (- v n))
      (with-slot (his-slot *opponent* (wrap-slot j))
        (when (alive? his-slot)
          (let ((w (slot-vitality his-slot))
                (hit (truncate (/ (* 9 n) 10))))
            (if (not *zombies-turn*)
                                        ; normal operation
                (if (> hit w)
                    (setf (slot-vitality his-slot) 0)
                    (setf (slot-vitality his-slot) (- w hit)))
                                        ; zombie operation
                (if (> (+ hit w) *max-live*)
                    (setf (slot-vitality his-slot) *max-live*)
                    (setf (slot-vitality his-slot) (+ hit w))))
            )))))
  @I)

(defcombinator help (i j n)
  (with-slot (slot-i *proponent* i)
    (let ((v (slot-vitality slot-i)))
      (in-game-check (and (integerp n) (<= n v)) "in call to help, n is ~A, v is ~A" n v)
      (with-slot (slot-j *proponent* j)
        (when (alive? slot-j)
          (let ((w (slot-vitality slot-j))
                (heal (truncate (/ (* n 11) 10))))
            (if (not *zombies-turn*)
                                        ; normal operation
                (if (> (+ heal w) *max-live*)
                    (setf (slot-vitality slot-j) *max-live*)
                    (setf (slot-vitality slot-j) (+ heal w)))
                                        ; zombie operation
                (if (< (- w heal) 0)
                    (setf (slot-vitality slot-j) 0)
                    (setf (slot-vitality slot-j) (- w heal))))
            )))))
  @I)

(defcombinator copy (i)
  (with-slot (slot *opponent* i)
    (slot-field slot)))

(defcombinator revive (i)
  (with-slot (slot *proponent* i)
    (when (<= (slot-vitality slot) 0)
      (setf (slot-vitality slot) 1)))
  @I)

(defcombinator zombie (i x)
  (with-slot (slot *opponent* (wrap-slot i))
    (in-game-check (dead? slot) "in call to zombie, i is ~A" i)
    (setf (slot-field slot) x)
    (setf (slot-vitality slot) -1))
  @I)

(defvar *last-application* nil)

(defun left-application (player card slot-number)
  ;;(format t "la: ~A -> ~A~%" card slot-number)
  (setf *last-application* (list 'left (cs-name (cb-spec card)) slot-number))
  (when (valid-slot-number? slot-number)                  ;TODO: what todo if it is not valid?
    (handler-case 
        (with-alive-slot (slot player slot-number)  ; note: as a side effect of this dead slots could be set to I
          (setf (slot-field slot) (cb-call card (slot-field slot))))
      (game-logic-error (_) 
        (declare (ignore _)) 
        (setf (slot-field (get-slot player slot-number)) @I)))))

(defun right-application (player slot-number card)
  (setf *last-application* (list 'right slot-number (cs-name (cb-spec card))))
  (when (valid-slot-number? slot-number)                  ;TODO: what todo if it is not valid?
    (handler-case 
        (with-alive-slot (slot player slot-number)  ; note: as a side effect of this dead slots could be set to I
          (setf (slot-field slot) (cb-call (slot-field slot) card)))
      (game-logic-error (_) 
        (declare (ignore _))
        (setf (slot-field (get-slot player slot-number)) @I)))))

(defun zombies-turn (player)
  (let ((*zombies-turn* t))
    (loop for slot-index from 0 to (1- *max-slots*) do
         (with-slot (slot player slot-index)
           (when (= (slot-vitality slot) -1)
             (handler-case (cb-call (slot-field slot) @I)
               (game-logic-error (_) (declare (ignore _))))
             (setf (slot-vitality slot) 0)
             (setf (slot-field slot)    @I)
             )))))

(defun card-function (symbol)
  (make-combinator :spec (gethash symbol *combinators*)))

(defun read-card ()
  (format t "   enter card: ")
  (finish-output)
  (card-function (intern (string-upcase (read-line)))))

(defun read-slot ()
  (format t "   enter slot: ")
  (finish-output)
  (parse-integer (read-line)))

(defun move (side)
  (with-applications-limit
    (zombies-turn side))
  (format t "Turn ~A~%" *cur-turn*)
  (format t "[~A] enter action code (1 - left, 2 - right): " (if (eq *proponent* side) "proponent" "opponent"))
  (finish-output)
  (with-applications-limit
    (case (parse-integer (read-line))
      (1 (left-application  side (read-card) (read-slot)))
      (2 (right-application side (read-slot) (read-card)))
      (t (error "wrong command"))))
  (format t "---- move end, status: ----~%")
  (format t "[proponent]: ~A~%" *proponent*)
                                        ;    (format t "[opponent]: ~A~%" *opponent*)
  (format t "---- status end ----~%~%"))

(defun silent-read-card ()
  (card-function (intern (string-upcase (read-line)))))

(defun silent-read-slot ()
  (parse-integer (read-line)))

(defun silent-move (side)
  (with-applications-limit
    (zombies-turn side))
  (with-applications-limit
    (case (parse-integer (read-line))
      (1 (left-application  side (silent-read-card) (silent-read-slot)))
      (2 (right-application side (silent-read-slot) (silent-read-card)))
      (t (error "wrong command")))))

(defun silent-move-from-command (side cmd)
  (with-applications-limit
    (zombies-turn side))
  (with-applications-limit
    (case (first cmd)
      (left (left-application   side (card-function (second cmd)) (third cmd)))
      (right (right-application side (third cmd) (card-function (second cmd))))
      (t (error "wrong command")))))

;;(defun game-loop (first second)
;;    (move first)
;;    (incf *cur-turn*)
;;    (move second)
;;    (game-loop first second))

;; (defun main ()
;;   (setf *cur-turn* 0)
;;   (if (string= (first #+:clisp *args* #+:sbcl sb-ext:*posix-argv*) "1")
;;       (game-loop *opponent* *proponent*)
;;       (game-loop *proponent* *opponent*)))

(defun game-loop (&optional (read-opponent t))
  ;;(format t ">> ~A ~A~%" *cur-turn* *max-turns*)
  (when (< *cur-turn* *max-turns*)
      (when read-opponent
        ;(format *error-output* "Reading opponent command~%")
        (silent-move *opponent*))
      ;(format *error-output* "Ok opponent command -~A~%" *last-application*)
      (let ((cmd (get-next-command)))
        ;(format *error-output* "Player command : ~A, turn ~A~%" cmd *cur-turn*)
        (silent-move-from-command *proponent* cmd)
        (handler-case (print-command cmd)
            (stream-error (_) (declare (ignore _)))))
      (incf *cur-turn*)
      (game-loop)))

(defun main ()
  (setf *cur-turn* 0)
  ;(format *error-output* "Running main with args ~A~%" sb-ext:*posix-argv*)
;  (ignore-errors
    (if (string= (second sb-ext:*posix-argv*) "0")
        (game-loop nil)
        (game-loop)));)

                                        ;(main)
