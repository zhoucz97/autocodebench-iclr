(defun find-substring-index (haystack needle)
  "Finds the first occurrence of the substring 'needle' within the string 'haystack'.
   Returns the index of the first character of the first occurrence of 'needle'.
   If 'needle' is not a part of 'haystack', returns -1."
  (let ((haystack-length (length haystack))
        (needle-length (length needle)))
    (if (zerop needle-length)
        0  ; empty string is considered to be at position 0
        (loop for i from 0 to (- haystack-length needle-length)
              when (string= (subseq haystack i (+ i needle-length)) needle)
              return i
              finally (return -1)))))


(defun test-find-substring-index ()
(assert (equal (find-substring-index "apple pie" "pie") 6))
(assert (equal (find-substring-index "hello world" "bye") -1))
(assert (equal (find-substring-index "common lisp" "lisp") 7))
(assert (equal (find-substring-index "quick brown fox" "brown") 6))
(assert (equal (find-substring-index "abcdef" "de") 3))
(assert (equal (find-substring-index "abcdef" "xyz") -1)))

(test-find-substring-index)