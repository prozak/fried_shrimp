(defvar *max-slots* 2)
(defvar *max-live*  65535)
(defvar *init-live* 10000)

(defun I (x) x)
#|
(defstruct (combinator-spec (:conc-name cs-))
    (eval-function nil :type function)
    (params-number 1   :type (integer 1 100)))

(defstruct (combinator (:type list) (:conc-name cb-))
    spec
    params)

(defun cb-apply (combinator param)
    (push param (cb-params combinator))
    (let* ((spec   (cb-spec combinator))
           (params (cb-params combinator)))
        (format t "cba:")
        (if (= (cs-params-number spec) (length params))
            (apply (cs-eval-function spec) (reverse params))
            combinator)))

; combinator symbol -> combinator spec
(defvar *combinators* (make-hash-table))

(defmacro defcombinator (name (&rest params) &body body)
    (when (< (length params) 1)
        (error "error while defining combinator: ~A, number of parameters should not be less then 1" name))
    `(let ()
        (defvar ,name (make-combinator-spec
                         :eval-function (lambda (,@params) ,@body)
                         :params-number ,(length params)))
        (setf (gethash ',name *combinators*) ,name)))

(defcombinator I (x) x)
(defcombinator test (x y z) (format t "test: ~A, ~A, ~A~%" x y z))

(format t "~S~%" *combinators*)

(let ((ci (make-combinator :spec I)))
    (cb-apply ci 1)
    (cb-apply ci 2)
    (cb-apply ci 3))
|#

(defstruct slot
    (field    #'I         );:type (or (integer 0 *max-live*) function))
    (vitality *init-live* ));:type (integer -1 *max-live*)))

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
    ;;(format t "gs: slots are: ~A~%" (player-slots player))
    (aref (player-slots player) slot-number))

(defmacro with-slot ((name player number) &body body)
    `(progn
        ;;(format t "ws: slot number ~A~%" ,number)
        (assert (valid-slot-number? ,number))
        (let ((,name (get-slot ,player ,number)))
            ;;(format t "ws: got slot: ~A~%" ,name)
            ,@body)))

(defmacro with-alive-slot ((name player number) &body body)
    `(with-slot (,name ,player ,number)
        (assert (alive? ,name))
        ,@body))

(defun zero (x) 0)

(defun succ (n)
    (assert (integerp n))
    (if (< n *max-live*)
        (1+ n)
        *max-live*))

(defun dbl (n)
    (assert (integerp n))
    (if (< n (/ *max-live* 2))
        (* 2 n)
        *max-live*))

(defun get-f (i)
    (with-alive-slot (slot *proponent* i)
        (slot-field slot)))

(defun put (unused)
    #'I)

(defun S (f)
    (assert (functionp f))
    (lambda (g)
        (assert (functionp g))
        (lambda (x) (let ((h (funcall f x))
                          (fake (assert (functionp h)))
                          (y (funcall g x))
                          (z (funcall h y)))
            z))))

(defun K (x)
    (lambda (y) x))

(defun inc (i)
    (with-slot (slot *proponent* i)
        (when (and (<  (slot-vitality slot) *max-live*)
                   (>= (slot-vitality slot) 0))
            (incf (slot-vitality slot))))
    #'I)

(defun dec (i)
    (with-slot (slot *opponent* (*max-slots* - 1 - i))
        (when (> (slot-vitality slot) 0)
            (decf (slot-vitality slot))))
    #'I)

(defun attack (i)
    (lambda (j)
        (lambda (n)
            (with-slot (my-slot *proponent* i)
                (let ((v (slot-vitality my-slot)))
                    (assert (<= n v))
                    (setf (slot-vitality my-slot) (- v n))
                    (with-slot (his-slot *opponent* (*max-slots* - 1 - j))
                        (when (alive? his-slot)
                            (let ((w (slot-vitality his-slot))
                                  (hit (truncate (/ (* 9 n) 10))))
                                (if (> hit w)
                                    (setf (slot-vitality his-slot) 0)
                                    (setf (slot-vitality his-slot) (- w hit))))))))
            #'I)))

(defun help (i)
    (lambda (j)
        (lambda (n)
            (with-slot (slot-i *proponent* i)
                (let ((v (slot-vitality slot-i)))
                    (assert (<= n v))
                    (whith-slot (slot-j *proponent* j)
                        (when (alive? slot-j)
                            (let ((w (+ (slot-vitality slot-j) (truncate (/ (* n 11) 10)))))
                                (setf (slot-vitality slot-j) (if (> w *max-live*)
                                                                 *max-live*
                                                                 w))
                                #'I))))))))

(defun copy (i)
    (with-slot (slot *opponent* i)
        (slot-field slot)))

(defun revive (i)
    (with-slot (slot *proponent* i)
        (when (<= (slot-vitality slot) 0)
            (setf (slot-vitality slot) 1)))
    #'I)

(defun zombie (i)
    (lambda (x)
        (with-slot (slot *opponent* (- (1- *max-slots*) i))
            (assert (dead? slot))
            (setf (slot-field slot) x)
            (setf (slot-vitality slot) -1))
        #'I))

(defun left-application (player card slot-number)
    ;;(format t "la: card is ~A~%" card)
    (assert (functionp card))
    (with-alive-slot (slot player slot-number)
        (setf (slot-field slot) (funcall card (slot-field slot)))))

(defun right-application (player slot-number card)
    (with-alive-slot (slot player slot-number)
        (assert (functionp (slot-field slot)))
        (setf (slot-field slot) (funcall (slot-field slot) card))))

(defun card-function (symbol)
    (case symbol
        ('|I| #'I)
        ('|zero| #'zero)
        ('|succ| #'succ)
        ('|dbl| #'dbl)
        ('|get| #'get-f)
        ('|put| #'put)
        ('|S| #'S)
        ('|K| #'K)
        ('|inc| #'inc)
        ('|dec| #'dec)
        ('|attack| #'attack)
        ('|help| #'help)
        ('|copy| #'copy)
        ('|revive| #'revive)
        ('|zombie| #'zombie)
        (t (error "wrong card: ~A" symbol))))

(defun read-card ()
    (format t "   enter card: ")
    (card-function (intern (read-line))))

(defun read-slot ()
    (format t "   enter slot: ")
    (parse-integer (read-line)))

(defun move (side)
    (format t "[~A] enter action code (1 - left, 2 - right): " (if (eq *proponent* side) "proponent" "opponent"))
    (case (parse-integer (read-line))
        (1 (left-application  side (read-card) (read-slot)))
        (2 (right-application side (read-slot) (read-card)))
        (t (error "wrong command")))
    (format t "---- turn end, status ----~%")
    (format t "[proponent]: ~A~%" *proponent*)
    (format t "[opponent]: ~A~%" *opponent*)
    (format t "---- status end ----~%~%"))

(defun game-loop (first second)
    (move first)
    (move second)
    (game-loop second first))

(defun main ()
    (if (string= (first *args*) "0")
        (game-loop *proponent* *opponent*)
        (game-loop *opponent* *proponent*)))

(main)
