(declaim (special *cur-turn*))

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

(defun select-task ()
    (make-task :commands-list (list (list 'left 'I 0))))

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
