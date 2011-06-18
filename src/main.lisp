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

(defun make-health-booster-task (slot-number amount)
    (destructuring-bind (tmp-slot) (find-slots *proponent* :slots-number 1
                                                           :score-fn #'slot-score
                                                           :not-use-list (list slot-number))
        (debug-format ">>> ~A ~A ~A~%" slot-number amount tmp-slot)
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

(defun make-attack-task (attacker-slot-number attacked-slot-number amount)
    (destructuring-bind (tmp-slot) (find-slots *proponent* :slots-number 1
                                                           :score-fn #'slot-score)
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
                                            (when (> v 0)
                                                (+ (/ v 100) (- 16 d)))))
                            :test-fn #'>)))


(defun select-task ()
    ;; (let ((slot-to-heal (find-slot-to-heal *proponent*)))
    ;;     (if slot-to-heal
    ;;        (make-health-booster-task slot-to-heal (1- (slot-vitality (get-slot *proponent* slot-to-heal))))
    ;;        (list 'left 'I 0)))
  (let* ((attacker (find-attacker-slot *proponent*))
         (attacked (find-slot-to-attack *opponent*))
         (attacker-v (slot-vitality (get-slot *proponent* attacker)))
         (attacked-v (slot-vitality (get-slot *opponent* attacked))))
    (if (and attacker attacked)
        (make-attack-task attacker attacked (if (< attacker-v (truncate (* attacked-v 10/9)))
                                                (truncate (* attacker-v 0.9))
                                                attacked-v))
        (list 'left 'I 0))))
;    (if *last-application*
;        (make-task :commands-list (list *last-application*))
;        (make-task :commands-list (list (list 'left 'I 0)))))
;    (make-task :commands-list (compile-lambda '(progn (set 0 (loop zz (inc zz)))
;                                                      (set 0 (get '1))
;                                                      (set 0 (0 zero)))
;                                              :target-slot 2
;                                              :prealloc-slots '(1))
;               :required-slots '(1)))
;    (make-task :commands-list (list (list 'left 'I 0))))

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
