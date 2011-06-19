(declaim (special *cur-turn*))

(defvar *debug* nil)
(defvar *current-task* nil)

(defstruct task
    commands-list   ; rest of the commands to do
    required-slots
    (complete-check-function (lambda () t)))

(defun task-complete? (task)
    (null (task-commands-list task)))

(defun get-needed-slots (commands)
    (remove-duplicates (mapcar #'third commands)))

(defun could-complete? (player task)
    (and (task-complete-check-function task)
         (every (lambda (slot-index) (alive? (get-slot player slot-index)))
                (append (task-required-slots task)
                        (get-needed-slots (task-commands-list task))))))

(defun slot-score (slot index)
    (let ((v (slot-vitality slot))
          (d (num-difficulty index)))
        (when (and (> v 0)
                   (< 0 index)) d)))

(defun find-slots (player &key (slots-number nil) (test-fn #'<) (score-fn (lambda () 1)) (not-use-list nil))
    (let ((slots (sort
                    (remove nil
                        (loop for i from 0 to (1- (length (player-slots player))) collect
                            (unless (member i not-use-list)
                                (with-slot (slot player i)
                                    (let ((score (funcall score-fn slot i)))
                                        (when score
                                            (cons score i)))))))
                    test-fn
                    :key #'car)))
        (if slots-number
            (when (>= (length slots) slots-number)
                (mapcar #'cdr (subseq slots 0 slots-number)))
            (mapcar #'cdr slots))))

(defmacro deftask (name (&rest params) use      (&rest slots)
                                       not-use  (&rest not-use-list)
                                       required (&rest req-slots)
                        commands-list 
                   &key (complete-check-function '(lambda () t)))
     (declare (ignore use not-use required))
    ;(assert slots t "deftask requires at least one slot parameter")
    (let ((tmp-list (gensym)))
      `(defun ,name (,@params)
         (let ,slots
           (let ((,tmp-list (find-slots *proponent* 
                                        :slots-number ,(length slots)
                                        :score-fn #'slot-score
                                        :not-use-list (list ,@not-use-list ,@slots)))) 
               (if (< (length ,tmp-list)
                      ,(length slots)) 
                   nil
                   (destructuring-bind (,@slots) ,tmp-list
                     (handler-case
                         (make-task :commands-list (compile-lambda ,commands-list
                                                                   :target-slot ,(car slots)
                                                                   :prealloc-slots
                                                                   (append (list ,@not-use-list ,@slots)
                                                                           (find-slots *proponent*
                                                                                       :score-fn (lambda (slot i) (declare (ignore i)) 
                                                                                                         (when (dead? slot) 1)))))
                                    :required-slots (list ,@slots ,@req-slots)
                                    :complete-check-function ,complete-check-function)
                       (compile-error (_) 
                         (declare (ignore _))
                         (debug-format "Compilation of task ~A ~A failed~%" ',name (list ,@params))
                         nil)))))))))

(defun value-complexity (val)
  (if (integerp val)
      1
      (if (typep val 'combinator)
          (1+ (apply #'+ (mapcar #'value-complexity (cb-params val))))
          0)))

;(print (macroexpand-1
(deftask make-health-booster-task (slot-number amount)
                                  use (tmp-slot)
                                  not-use nil
                                  required (slot-number)
    `(progn (set ,tmp-slot (loop zz (help ',slot-number ,(if (= 0 slot-number)
                                                             'zz
                                                             `(K ',slot-number zz)) ',amount)))
            (set 0 (get ',tmp-slot))
            (set 0 (0 zero))));))
#|
(defun make-health-booster-task (slot-number amount)
    (destructuring-bind (tmp-slot) (find-slots *proponent* :slots-number 1
                                                           :score-fn #'slot-score
                                                           :not-use-list (list slot-number))
        (debug-format "Making health booster ~A ~A~%" slot-number amount)
        (make-task :commands-list
                    (compile-lambda
                        `(progn (set ,tmp-slot (loop zz (help ',slot-number ,(if (= 0 slot-number)
                                                                                 'zz
                                                                                 `(K ',slot-number zz)) ',amount)))
                                (set 0 (get ',tmp-slot))
                                (set 0 (0 zero)))
                        :target-slot tmp-slot
                        :prealloc-slots (append (list slot-number tmp-slot)
                                                (find-slots *proponent*
                                                            :score-fn (lambda (slot i) (when (dead? slot) 1)))))
                    :required-slots (list slot-number tmp-slot))))
|#

(deftask make-attack-task (attacker-slot-number attacked-slot-number amount)
                            use (tmp-slot)
                            not-use nil
                            required (attacker-slot-number)
      `(attack ',attacker-slot-number ',(- 255 attacked-slot-number) ',amount)
      :complete-check-function (lambda () (let ((v (slot-vitality (get-slot *opponent* attacked-slot-number)))
                                                (v2 (slot-vitality (get-slot *proponent* attacker-slot-number))))
                                            (and (> v 0)
                                                 (> v2 amount)))))

#|
(defun make-attack-task (attacker-slot-number attacked-slot-number amount)
    (destructuring-bind (tmp-slot) (find-slots *proponent* :slots-number 1
                                                           :score-fn #'slot-score)
        (debug-format "Making attacker ~A ~A ~A~%" attacker-slot-number  attacked-slot-number amount)
        (make-task :commands-list
                    (compile-lambda
                        `(attack ',attacker-slot-number ',(- 255 attacked-slot-number) ',amount)
                        :target-slot tmp-slot
                        :prealloc-slots (append (list tmp-slot)
                                                (find-slots *proponent*
                                                            :score-fn (lambda (slot i) (when (dead? slot) 1)))))
                    :required-slots (list tmp-slot attacker-slot-number)
                    :complete-check-function (lambda () (let ((v (slot-vitality (get-slot *opponent* attacked-slot-number)))
                                                              (v2 (slot-vitality (get-slot *proponent* attacker-slot-number))))
                                                          (and (> v 0)
                                                               (> v2 amount)))))))
|#
(defun find-slot-to-heal (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (> v 0)
                                                (+ (/ v 100) d)))))))

(defun find-slot-to-attack (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (> v 0)
                                                (+ (/ v 100) (- 16 d))))))))

(defun find-slots-to-attack-2 (player num-slots)
  (find-slots player 
              :slots-number num-slots
              :score-fn (lambda (slot index)
                          (let ((v (slot-vitality slot))
                                (d (num-difficulty index)))
                            (when (> v 0)
                              (+ (/ v 100) d))))
              :test-fn #'>))

(defun find-attacker-slot (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (and (> v 0) (> index 0))
                                                (+ (/ v 100) (- 16 d)))))
                            :test-fn #'>)))

(deftask make-helper-task (helper-slot-number helped-slot-number amount)
                          use (tmp-slot)
                          not-use nil
                          required (helped-slot-number helped-slot-number)
              `(help ',helper-slot-number ',helped-slot-number ',amount)
              :complete-check-function (lambda () (let ((v (slot-vitality (get-slot *proponent* helper-slot-number)))
                                                        (v2 (slot-vitality (get-slot *proponent* helped-slot-number))))
                                                    (and (> v2 0)
                                                         (> v amount)))))
#|
(defun make-helper-task (helper-slot-number helped-slot-number amount)
    (destructuring-bind (tmp-slot) (find-slots *proponent* :slots-number 1
                                                           :score-fn #'slot-score)
        (debug-format "Making helper ~A ~A ~A~%" helper-slot-number  helped-slot-number amount)
        (make-task :commands-list
                    (compile-lambda
                        `(help ',helper-slot-number ',helped-slot-number ',amount)
                        :target-slot tmp-slot
                        :prealloc-slots (append (list tmp-slot)
                                                (find-slots *proponent*
                                                            :score-fn (lambda (slot i) (when (dead? slot) 1)))))
                    :required-slots (list tmp-slot helper-slot-number helped-slot-number)
                    :complete-check-function (lambda () (let ((v (slot-vitality (get-slot *proponent* helper-slot-number)))
                                                              (v2 (slot-vitality (get-slot *proponent* helped-slot-number))))
                                                          (and (> v2 0)
                                                               (> v amount)))))))
|#

(defun find-helper-slot (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (and (> v 0) (> index 0))
                                                (+ (/ v 100) (- 16 d)))))
                            :test-fn #'>)))

(defun find-zombie-helper-slot (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (and (> v 0) (> index 0))
                                                (+ (/ v 100) d))))
                            :test-fn #'>)))

(deftask make-reviver-task (revive-slot-number)
                           use (tmp-slot)
                           not-use nil
                           required nil
                           `(revive ',revive-slot-number)
                           :complete-check-function (lambda () (let ((v (slot-vitality (get-slot *proponent* revive-slot-number))))
                                                                 (<= v 0))))

(deftask make-zombie-helper-task (zombie-slot-number from-slot-number to-slot-number amount)
                           use (tmp-slot)
                           not-use nil
                           required nil
                           `(zombie ',(- 255 zombie-slot-number) (lambda (id) (help (id ',from-slot-number) ',to-slot-number ',amount)))
                           :complete-check-function (lambda () (let ((v (slot-vitality (get-slot *opponent* zombie-slot-number))))
                                                                 (<= v 0))))

(deftask make-zombie-attacker-task (zombie-slot-number from-slot-number to-slot-number amount)
                           use (tmp-slot)
                           not-use nil
                           required (to-slot-number)
                           `(zombie ',(- 255 zombie-slot-number) (lambda (id) (attack (id ',from-slot-number) ',(- 255 to-slot-number) ',amount)))
                           :complete-check-function (lambda () (let ((v (slot-vitality (get-slot *opponent* zombie-slot-number))))
                                                                 (<= v 0))))

#|
(defun make-reviver-task (revive-slot-number)
    (destructuring-bind (tmp-slot) (find-slots *proponent* :slots-number 1
                                                           :score-fn #'slot-score)
        (debug-format "Making reviver ~A~%" revive-slot-number)
        (make-task :commands-list
                    (compile-lambda
                        `(revive ',revive-slot-number)
                        :target-slot tmp-slot
                        :prealloc-slots (append (list tmp-slot)
                                                (find-slots *proponent*
                                                            :score-fn (lambda (slot i) (when (dead? slot) 1)))))
                    :required-slots (list tmp-slot revive-slot-number)
                    :complete-check-function (lambda () (let ((v (slot-vitality (get-slot *proponent* revive-slot-number))))
                                                          (<= v 0))))))
|#

(defun find-slot-to-revive (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (<= v 0)
                                                d))))))

(defun find-slot-to-zombie (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (= v 0)
                                                (- 16 d)))))))

(defun find-slot-that-needs-healing (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (and (> v 0) (<= v 1000))
                                                (+ (/ v 100) d)))))))

(defun need-to-kill-zero-slot (player)
  (> (slot-vitality (get-slot player 0))
     0))

(defun need-to-heal-zero-slot (player)
  (<= (slot-vitality (get-slot player 0))
     2000))

(defun need-to-boost-zero-slot (player)
  (<= (slot-vitality (get-slot player 0))
     30000))

(defun make-attacker (&optional (slot (find-attacker-slot *proponent*)))
  (let* ((attacker slot)
         (attacked (find-slot-to-attack *opponent*)))
    (when (and attacker attacked)
      (let ((attacker-v (slot-vitality (get-slot *proponent* attacker)))
            (attacked-v (slot-vitality (get-slot *opponent* attacked))))
        (make-attack-task attacker attacked (if (< attacker-v (truncate (* attacked-v 10/9)))
                                                (truncate (* attacker-v 0.9))
                                                (ceiling (* attacked-v 10/9))))))))

(defun make-helper (&optional (slot (find-slot-to-heal *proponent*)))
  (let* ((helper (find-helper-slot *proponent*))
         (healed slot))
    (when (and helper healed)
      (let ((helper-v (slot-vitality (get-slot *proponent* helper)))
            (healed-v (slot-vitality (get-slot *proponent* healed))))
        (make-helper-task helper healed (if (< helper-v (truncate (* healed-v 10/11)))
                                            (truncate (* helper-v 0.9))
                                            (truncate (* helper-v 0.5))))))))

(defun make-health-booster (&optional (slot (find-slot-to-heal *proponent*)))
    (let ((slot-to-heal slot))
        (when slot-to-heal
          (make-health-booster-task slot-to-heal (1- (slot-vitality (get-slot *proponent* slot-to-heal)))))))

(defun make-zombier (&optional (target-slot nil) (slot (find-slot-to-zombie *opponent*)))
  (let* ((a-slots (find-slots-to-attack-2 *opponent* 2))
         (helper (or target-slot (first a-slots)))
         (healed (if target-slot 
                     (first a-slots) 
                     (second a-slots))))
    (when (and helper healed slot)
      (let ((helper-v (slot-vitality (get-slot *opponent* helper)))
            (healed-v (slot-vitality (get-slot *opponent* healed))))
        (make-zombie-helper-task slot helper healed helper-v)))))

(defun make-zombier-attacker (&optional (recipient (find-slot-to-heal *proponent*)) (slot (find-slot-to-zombie *opponent*)))
  (let* ((donor (find-zombie-helper-slot *opponent*)))
    (when (and donor recipient slot)
      (let ((donor-v (slot-vitality (get-slot *opponent* donor)))
            (recipient-v (slot-vitality (get-slot *proponent* recipient))))
        (make-zombie-attacker-task slot donor recipient donor-v)))))

(defun make-adv-helper (slot)
  (let ((zombie-helper (make-zombier-attacker slot)))
    (if zombie-helper
        zombie-helper
        (make-helper slot))))

(defconstant +heal-ratio+ 20)
(defconstant +zombie-ratio+ 90)

(defun select-task ()
  (let* ((revive (find-slot-to-revive *proponent*))
         (to-heal (find-slot-that-needs-healing *proponent*))
         (rnd (random 100))
         (cmd (or (and (and revive (= revive 0))
                       (make-reviver-task revive))
                  (and (and (< rnd 20)
                            (need-to-kill-zero-slot *opponent*))
                       (or (make-zombier 0)
                           (make-attacker 0)))
                  (and (need-to-heal-zero-slot *proponent*)
                       (make-adv-helper 0))
                  (and (and (< rnd +heal-ratio+)
                            revive)
                       (make-reviver-task revive))
                  (and (and (< rnd +heal-ratio+)
                            (and to-heal (/= 0 to-heal)))
                       (make-adv-helper to-heal))
                  (and (and (< rnd +heal-ratio+)
                            (need-to-boost-zero-slot *proponent*))
                       (make-health-booster 0))
                  (and (< rnd +heal-ratio+)
                       (make-health-booster))
                  (and (< rnd +zombie-ratio+)
                       (make-zombier))
                  (make-attacker))))
    (if cmd
        cmd
        (make-task :commands-list (list (list 'left 'I 0))))))          

(defvar *get-next-command-call-count* 0)       

(defun get-next-command ()
  ;;(format t "gnc: task is ~A~%" *current-task*)
  (incf *get-next-command-call-count*)
  (when (> *get-next-command-call-count* 100)
    (debug-format "get-next-command loop detected~%")
    (setf *current-task* (make-task :commands-list (list (list 'left 'I 0)))))
  (if (and *current-task*
           (not (task-complete? *current-task*))
           (could-complete? *proponent* *current-task*))
      (pop (task-commands-list *current-task*))
      (progn
        (setf *current-task* (ignore-errors (select-task)))
        (unless *current-task*
          (setf *current-task* (make-task :commands-list (list (list 'left 'I 0)))))
                                        ;(format t "gnc: task after selection: ~A, complete? ~A, could complete? ~A~%"
                                        ;     *current-task*
                                        ;     (task-complete? *current-task*)
                                        ;     (could-complete? *proponent* *current-task*))
        (get-next-command))))

;;(main)
