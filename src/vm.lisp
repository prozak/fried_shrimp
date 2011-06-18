(defvar *max-slots* 2)
(defvar *max-live*  65535)
(defvar *init-live* 10000)
(defvar *max-move-applications-number* 1000)
(defvar *current-move-applications-count* 0)

(defmacro with-applications-limit (&body body)
    `(let ((*current-move-applications-number* 0))
        ,@body))

(defstruct (combinator-spec (:conc-name cs-))
    (name          'noname  :type symbol)
    (eval-function nil      :type function)
    (params-number 1        :type (integer 1 100)))

(defstruct (combinator (:conc-name cb-)
                       (:print-function (lambda (self stream _)
                                          (let ((spec (cb-spec self))
                                                (params (cb-params self)))
                                            (format stream "[~A ~A]"
                                                (cs-name spec)
                                                (append (reverse params)
                                                        (loop for _ from 1 to (- (cs-params-number spec) (length params)) collect
                                                            "_")))))))
    spec
    params)

; defines '@' to be used to create new combinator instance, the syntax is:
;   @<some combinator defined by 'defcombinator'>
(set-macro-character #\@ (lambda (stream char)
                            (list 'make-combinator ':spec (read stream t nil t))))

; raised for 'errors' specified in the doc
(define-condition game-logic-error (error) ())

(defmacro in-game-check (condition &optional comment &rest comment-params)
    `(unless ,condition
        (format t "BANG: in game check ~A failed~A" ',condition (if ,comment ": " ""))
        (when ,comment
            (format t ,comment ,@comment-params))
        (format t "~%")
        (error 'game-logic-error)))

(defun cb-call (combinator param)
    (in-game-check (typep combinator 'combinator) "in call to cb-call")
    (in-game-check (> (incf *current-move-applications-count*)
                      *max-move-applications-number*)
                   "in call to cb-call")
    (push param (cb-params combinator))
    (let* ((spec                (cb-spec combinator))
           (params              (cb-params combinator))
           (required-params-num (cs-params-number spec))
           (actual-params-num   (length params)))
        ;(format t "cba: ~A~%" combinator)
        (cond
            ((< actual-params-num required-params-num) combinator)
            ((= actual-params-num required-params-num)
                (apply (cs-eval-function spec) (reverse params)))
            (t (error "cb-apply called with more parameters then expected, combinator: ~A" combinator)))
        ))

; combinator symbol -> combinator spec
(defvar *combinators* (make-hash-table))

(defmacro defcombinator (name (&rest params) &body body)
    (when (< (length params) 1)
        (error "error while defining combinator: ~A, number of parameters should not be less then 1" name))
    `(let ()
        (setf ,name (make-combinator-spec
                         :name ',name
                         :eval-function (lambda (,@params) ,@body)
                         :params-number ,(length params)))
        (setf (gethash ',name *combinators*) ,name)))

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

(defstruct (slot (:print-function (lambda (self stream _)
                                    (format stream "{~A: ~A}" (slot-vitality self) (slot-field self)))))
    (field    @I         );:type (or (integer 0 *max-live*) function))
    (vitality *init-live*));:type (integer -1 *max-live*)))

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
    (slots (fill-array (make-array *max-slots* :element-type 'slot) #'make-slot)))

(defvar *proponent* (make-player))
(defvar *opponent*  (make-player))

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

(defcombinator zero (x) 0)

(defcombinator succ (n)
    (in-game-check (integerp n) "in call to succ, n is ~A" n)
    (if (< n *max-live*)
        (1+ n)
        *max-live*))

(defcombinator dbl (n)
    (in-game-check (integerp n) "in call to dbl, n is ~A" n)
    (if (< n (/ *max-live* 2))
        (* 2 n)
        *max-live*))

(defcombinator get (i)
    (with-alive-slot (slot *proponent* i)
        (slot-field slot)))

(defcombinator put (unused) @I)

(defcombinator S (f g x)
    (let ((h (cb-call f x))
          (y (cb-call g x))
          (z (cb-call h y)))
        z))

(defcombinator K (x y) x)

(defcombinator inc (i)
    (with-slot (slot *proponent* i)
        (when (and (<  (slot-vitality slot) *max-live*)
                   (>= (slot-vitality slot) 0))
            (incf (slot-vitality slot))))
    @I)

(defun wrap-slot (number)
    (- (1- *max-slots*) number))

(defcombinator dec (i)
    (with-slot (slot *opponent* (wrap-slot i))
        (when (> (slot-vitality slot) 0)
            (decf (slot-vitality slot))))
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
                        (if (> hit w)
                            (setf (slot-vitality his-slot) 0)
                            (setf (slot-vitality his-slot) (- w hit))))))))
            @I)

(defcombinator help (i j n)
    (with-slot (slot-i *proponent* i)
        (let ((v (slot-vitality slot-i)))
            (in-game-check (and (integerp n) (<= n v)) "in call to help, n is ~A, v is ~A" n v)
            (whith-slot (slot-j *proponent* j)
                (when (alive? slot-j)
                    (let ((w (+ (slot-vitality slot-j) (truncate (/ (* n 11) 10)))))
                        (setf (slot-vitality slot-j) (if (> w *max-live*)
                                                         *max-live*
                                                         w))
                        @I))))))

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

(defun left-application (player card slot-number)
    ;;(format t "la: ~A -> ~A~%" card slot-number)
    (when (valid-slot-number? slot-number)                  ;TODO: what todo if it is not valid?
        (handler-case 
                (with-alive-slot (slot player slot-number)  ; note: as a side effect of this dead slots could be set to I
                    (setf (slot-field slot) (cb-call card (slot-field slot))))
            (game-logic-error (_) (setf (slot-field (get-slot player slot-number)) @I)))))

(defun right-application (player slot-number card)
    (when (valid-slot-number? slot-number)                  ;TODO: what todo if it is not valid?
        (handler-case 
                (with-alive-slot (slot player slot-number)  ; note: as a side effect of this dead slots could be set to I
                    (setf (slot-field slot) (cb-call (slot-field slot) card)))
            (game-logic-error (_) (setf (slot-field (get-slot player slot-number)) @I)))))

(defun zombies-turn (player)
    (loop for slot-index from 0 to (1- *max-slots*) do
        (with-slot (slot player slot-index)
            (when (= (slot-vitality slot) -1)
                  (handler-case (cb-call (slot-field slot) @I)
                    (game-logic-error (_)))
                  (setf (slot-vitality slot) 0)
                  (setf (slot-field slot)    @I)
                ))))

(defun card-function (symbol)
    @(gethash symbol *combinators*))

(defun read-card ()
    (format t "   enter card: ")
    (card-function (intern (string-upcase (read-line)))))

(defun read-slot ()
    (format t "   enter slot: ")
    (parse-integer (read-line)))

(defun move (side)
    (with-applications-limit
        (zombies-turn side))
    (format t "[~A] enter action code (1 - left, 2 - right): " (if (eq *proponent* side) "proponent" "opponent"))
    (with-applications-limit
        (case (parse-integer (read-line))
            (1 (left-application  side (read-card) (read-slot)))
            (2 (right-application side (read-slot) (read-card)))
            (t (error "wrong command"))))
    (format t "---- move end, status: ----~%")
    (format t "[proponent]: ~A~%" *proponent*)
    (format t "[opponent]: ~A~%" *opponent*)
    (format t "---- status end ----~%~%"))

(defun game-loop (first second)
    (move first)
    (move second)
    (game-loop first second))

(defun main ()
    (if (string= (first *args*) "0")
        (game-loop *proponent* *opponent*)
        (game-loop *opponent* *proponent*)))

(main)
