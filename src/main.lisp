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

(defun select-task ()
    (if *last-application*
        (make-task :commands-list (list *last-application*))
        (make-task :commands-list (list (list 'left 'I 0)))))
;    (make-task :commands-list (compile-lambda '(progn (set 1 (loop zz (inc zz)))
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
