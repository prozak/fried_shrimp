(defvar *cards*
  (list "I" "zero" "succ" "dbl" "get" "put" "S" "K" "inc" "dec" "attack" "help" "copy" "revive" "zombie"))

(defun test ()
  (with-open-file (stream "test.log" :direction :output :if-exists :supersede)
    (let* ((rand-state (make-random-state t))
           (slot (random 255 rand-state))
           (card (nth (random (length *cards*) rand-state) *cards*))
           (direction (1+ (random 2 rand-state))))
      (format t "~A~%" direction)
      (format stream "~A~%" direction)
      (if (= direction 1)
        (progn
          (format t "~A~%~A~%" card slot)
          (format stream "~A~%~A~%" card slot)
          )
        (progn
          (format t "~A~%~A~%" slot card)
          (format stream "~A~%~A~%" slot card))))))

(defun run-test(i)
  (test)
  (if (< i 100000)
    (run-test (1+ i))))

(defun test-it () (run-test 0))
