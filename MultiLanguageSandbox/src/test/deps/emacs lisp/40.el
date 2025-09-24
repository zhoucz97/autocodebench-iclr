(require 'cl-lib)

(defun median-after-removing-extremes (numbers-list)
  "Calculate the median of a list after removing the maximum and minimum values.
If the list is empty or contains less than 3 elements, return nil."
  (if (< (length numbers-list) 3)
      nil
    (let* ((sorted (sort (copy-sequence numbers-list) '<))
           (min-val (car sorted))
           (max-val (car (last sorted)))
           (filtered (remove min-val (remove max-val sorted))))
      (let ((len (length filtered)))
        (if (zerop (mod len 2))
            (/ (+ (nth (/ len 2) filtered)
                  (nth (- (/ len 2) 1) filtered))
               2.0)
          (nth (/ (1- len) 2) filtered))))))


(defun check-median-after-removing-extremes ()
  (cl-assert (equal (median-after-removing-extremes '(5 3 1 4 2)) 3))
  (cl-assert (equal (median-after-removing-extremes '(1 2 3)) 2))
  (cl-assert (equal (median-after-removing-extremes '(1)) nil))
  (cl-assert (equal (median-after-removing-extremes '(7 5 3 9 1 6)) 5))
  (cl-assert (equal (median-after-removing-extremes '(10 20 30 40 50)) 30)))

(check-median-after-removing-extremes)