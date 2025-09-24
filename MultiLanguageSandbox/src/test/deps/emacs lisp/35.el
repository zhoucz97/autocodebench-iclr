(require 'cl-lib)

(defun construct-fbi-tree (n binary-string)
  "Construct an FBI tree from a binary string of length 2^N and return its postorder traversal."
  (let ((len (length binary-string)))
    (if (= len 1)
        (if (string= binary-string "0") "B" "I")
      (let* ((half (/ len 2))
             (left-half (substring binary-string 0 half))
             (right-half (substring binary-string half))
             (left-traversal (construct-fbi-tree n left-half))
             (right-traversal (construct-fbi-tree n right-half))
             (left-char (substring left-traversal -1))
             (right-char (substring right-traversal -1))
             (current-char (cond
                            ((and (string= left-char "B") (string= right-char "B")) "B")
                            ((and (string= left-char "I") (string= right-char "I")) "I")
                            (t "F"))))
        (concat left-traversal right-traversal current-char)))))


(defun test-construct-fbi-tree ()
(princ(construct-fbi-tree 2 "1010"))
(cl-assert (equal (construct-fbi-tree 2 "1010") "IBFIBFF"))
(cl-assert (equal (construct-fbi-tree 1 "01") "BIF"))
(cl-assert (equal (construct-fbi-tree 3 "10001011") "IBFBBBFIBFIIIFF"))
(cl-assert (equal (construct-fbi-tree 2 "1111") "IIIIIII")))

(test-construct-fbi-tree)