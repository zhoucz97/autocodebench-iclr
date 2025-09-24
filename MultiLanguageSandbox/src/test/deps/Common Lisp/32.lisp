(defun create-largest-number (numbers)
  (let ((sorted-numbers (sort (copy-list numbers) 
                              #'> :key (lambda (x) (format nil "~a" x)))))
    (let ((result ""))
      (dolist (num sorted-numbers result)
        (setf result (concatenate 'string result (format nil "~a" num)))))))


(defun test-create-largest-number ()
(assert (string= (create-largest-number '(56 9 45)) "95645"))
(assert (string= (create-largest-number '(5 50 56)) "56550"))
(assert (string= (create-largest-number '(3 34 302 50)) "50343302"))
(assert (string= (create-largest-number '(10 2 23)) "23210"))
(assert (string= (create-largest-number '(4 42 40 400)) "44240400")))

(test-create-largest-number)