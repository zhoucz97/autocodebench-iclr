(require 'cl-lib)

(defun find-shortest-subarray-with-same-degree (nums)
  "Find the length of the shortest subarray with the same degree as the original array.
NUMS is a list of non-negative integers.
Returns the length of the shortest subarray with the same degree."
  (let ((freq (make-hash-table :test 'equal))
        (first-pos (make-hash-table :test 'equal))
        (last-pos (make-hash-table :test 'equal))
        (max-freq 0)
        (min-length (length nums)))
    
    ;; First pass: record frequency, first and last positions of each number
    (dotimes (i (length nums))
      (let ((num (nth i nums)))
        (puthash num (1+ (gethash num freq 0)) freq)
        (unless (gethash num first-pos)
          (puthash num i first-pos))
        (puthash num i last-pos)))
    
    ;; Find the maximum frequency (degree of the array)
    (maphash (lambda (k v) (when (> v max-freq) (setq max-freq v))) freq)
    
    ;; Second pass: find the minimum length among elements with max frequency
    (maphash (lambda (k v)
               (when (= v max-freq)
                 (let ((len (- (gethash k last-pos) (gethash k first-pos) -1)))
                   (when (< len min-length)
                     (setq min-length len)))))
             freq)
    
    min-length))


(defun check-find-shortest-subarray-with-same-degree ()
(cl-assert (equal (find-shortest-subarray-with-same-degree '(1 2 2 3 1)) 2))
(cl-assert (equal (find-shortest-subarray-with-same-degree '(1 2 2 3 1 4 2)) 6))
(cl-assert (equal (find-shortest-subarray-with-same-degree '(1 2 3 4 5)) 1))
(cl-assert (equal (find-shortest-subarray-with-same-degree '(2 1 1 2 1 3 3 3 1 3)) 5))
(cl-assert (equal (find-shortest-subarray-with-same-degree '(1)) 1)))

(check-find-shortest-subarray-with-same-degree)