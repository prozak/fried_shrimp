(declaim (special *cur-turn*))

(defvar *debug* t)
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

(defmacro deftask (name (&rest params) use     (&rest slots)
                                       not-use (&rest not-use-list)
                        commands-list)
    ;(assert slots t "deftask requires at least one slot parameter")
    `(defun ,name (,@params)
       (let ,slots
        (destructuring-bind (,@slots) (find-slots *proponent* :slots-number ,(length slots)
                                                              :score-fn #'slot-score
                                                              :not-use-list (list ,@not-use-list ,@slots))
            (make-task :commands-list (compile-lambda ,commands-list
                                        :target-slot ,(car slots)
                                        :prealloc-slots
                                            (append (list ,@not-use-list ,@slots)
                                                    (find-slots *proponent*
                                                                :score-fn (lambda (slot i) (when (dead? slot) 1)))))
                       :required-slots (list ,@slots))))))
;(print (macroexpand-1
(deftask make-health-booster-task (slot-number amount)
                                  use (tmp-slot)
                                  not-use nil
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

(defun find-attacker-slot (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (and (> v 0) (> index 0))
                                                (+ (/ v 100) (- 16 d)))))
                            :test-fn #'>)))

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

(defun find-helper-slot (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (and (> v 0) (> index 0))
                                                (+ (/ v 100) (- 16 d)))))
                            :test-fn #'>)))

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
(defun find-slot-to-revive (player)
    (car (find-slots player :slots-number 1
                            :score-fn (lambda (slot index)
                                        (let ((v (slot-vitality slot))
                                              (d (num-difficulty index)))
                                            (when (<= v 0)
                                                d)))
                            :test-fn #'>)))

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
         (attacked (find-slot-to-attack *opponent*))
         (attacker-v (slot-vitality (get-slot *proponent* attacker)))
         (attacked-v (slot-vitality (get-slot *opponent* attacked))))
    (when (and attacker attacked)
      (make-attack-task attacker attacked (if (< attacker-v (truncate (* attacked-v 10/9)))
                                              (truncate (* attacker-v 0.9))
                                              attacked-v)))))

(defun make-helper (&optional (slot (find-slot-to-heal *proponent*)))
  (let* ((helper (find-helper-slot *proponent*))
         (healed slot)
         (helper-v (slot-vitality (get-slot *proponent* helper)))
         (healed-v (slot-vitality (get-slot *proponent* healed))))
    (when (and helper healed)
      (make-helper-task helper healed (if (< helper-v (truncate (* healed-v 10/11)))
                                          (truncate (* helper-v 0.9))
                                          (truncate (* helper-v 0.5)))))))

(defun make-health-booster (&optional (slot (find-slot-to-heal *proponent*)))
    (let ((slot-to-heal slot))
        (when slot-to-heal
          (make-health-booster-task slot-to-heal (1- (slot-vitality (get-slot *proponent* slot-to-heal)))))))

(defconstant +heal-ratio+ 30)

(defun select-task ()
  (let* ((revive (find-slot-to-revive *proponent*))
         (to-heal (find-slot-that-needs-healing *proponent*))
         (rnd (random 100))
         (cmd (or (and (and revive (= revive 0))
                       (make-reviver-task revive))
                  (and (and (< rnd 15)
                            (need-to-kill-zero-slot *opponent*))
                       (make-attacker 0))
                  (and (need-to-heal-zero-slot *proponent*)
                       (make-helper 0))
                  (and (and (< rnd +heal-ratio+)
                            revive)
                       (make-reviver-task revive))
                  (and (and (< rnd +heal-ratio+)
                            (and to-heal (/= 0 to-heal)))
                       (make-helper to-heal))
                  (and (and (< rnd +heal-ratio+)
                            (need-to-boost-zero-slot *proponent*))
                       (make-health-booster 0))
                  (and (< rnd +heal-ratio+)
                       (make-health-booster))
                  (make-attacker))))
    (if cmd
        cmd
        (list 'left 'I 0))))          
        

(defun get-next-command ()
    ;(format t "gnc: task is ~A~%" *current-task*)
    (if (and *current-task*
             (not (task-complete? *current-task*))
             (could-complete? *proponent* *current-task*))
        (pop (task-commands-list *current-task*))
        (progn
            (setf *current-task* (select-task))
            ;(format t "gnc: task after selection: ~A, complete? ~A, could complete? ~A~%"
            ;     *current-task*
            ;     (task-complete? *current-task*)
            ;     (could-complete? *proponent* *current-task*))
            (get-next-command))))

;;(main)
