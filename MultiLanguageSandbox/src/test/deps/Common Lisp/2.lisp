(defun bin-search (obj vec)
  "Binary search for OBJ in sorted vector VEC.
   Returns T if OBJ is found, NIL otherwise."
  (let ((low 0)
        (high (1- (length vec))))
    (loop while (<= low high) do
      (let* ((mid (floor (+ low high) 2))
             (mid-val (aref vec mid)))
        (cond
          ((= mid-val obj) (return t))
          ((< mid-val obj) (setf low (1+ mid)))
          (t (setf high (1- mid))))))
    nil))


(defun check-bin-search ()
;; Test the 'bin-search' function with various cases.
(assert (equal (bin-search 3 '#(1 2 3 4 5)) t))
(assert (equal (bin-search 6 '#(1 2 3 4 5)) nil))
(assert (equal (bin-search 1 '#(1)) t))
(assert (equal (bin-search 2 '#(1)) nil))
(assert (equal (bin-search 0 '#()) nil))
(assert (equal (bin-search 5 '#(2 4 6 8 10)) nil))
(assert (equal (bin-search 4 '#(2 4 6 8 10)) t)))

(check-bin-search)