(defun enqueue-front (object queue)
  (cons object queue))


(defun check-enqueue-front ()
(let ((queue '(2 3 4)))
(setq queue (enqueue-front 1 queue))
(assert (equal queue '(1 2 3 4))))
(let ((queue '()))
(setq queue (enqueue-front 'a queue))
(assert (equal queue '(a)))))

(check-enqueue-front)