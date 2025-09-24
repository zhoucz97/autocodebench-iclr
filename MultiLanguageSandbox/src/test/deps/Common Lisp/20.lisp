(defun move-first-instance-to-front (object queue)
  "Destructively moves the first instance of OBJECT (using EQL equivalence) to the front of QUEUE.
   Returns the modified queue."
  (let ((found nil)
        (prev nil)
        (current queue))
    (loop while (and current (not found))
          do (if (eql (car current) object)
                 (progn
                   (setf found t)
                   (if prev
                       (progn
                         (rplacd prev (cdr current))
                         (rplacd current queue)))
                   (return-from move-first-instance-to-front (cons object (cdr current))))
                 (progn
                   (setf prev current)
                   (setf current (cdr current)))))
    queue))  ; return original queue if object not found


(defun check-move-first-instance-to-front ()
(assert (equal (move-first-instance-to-front 'b '(a b c d)) '(b a c d)))
(assert (equal (move-first-instance-to-front 'a '(a b c d)) '(a b c d)))
(assert (equal (move-first-instance-to-front 'e '(a b c d)) '(a b c d)))
(assert (equal (move-first-instance-to-front 'd '(a b c d)) '(d a b c))))

(check-move-first-instance-to-front)