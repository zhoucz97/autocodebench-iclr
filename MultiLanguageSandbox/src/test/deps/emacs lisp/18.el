(require 'cl-lib)

(defun most-common-word (paragraph banned)
  "Find the most common non-banned word in PARAGRAPH.
PARAGRAPH is a string containing words.
BANNED is a list of strings representing banned words.
Returns the most common non-banned word in lowercase."
  (let ((word-counts (make-hash-table :test 'equal))
        (max-count 0)
        (result "")
        (words (split-string 
                (downcase 
                 (replace-regexp-in-string "[^a-zA-Z ]" "" paragraph)) 
                " " t)))
    (dolist (word words)
      (unless (member word banned)
        (let ((count (1+ (gethash word word-counts 0))))
          (puthash word count word-counts)
          (when (> count max-count)
            (setq max-count count)
            (setq result word)))))
    result))


(defun check-most-common-word ()
(cl-assert (equal (most-common-word "Bob hit a ball, the hit BALL flew far after it was hit." '("hit")) "ball"))
(cl-assert (equal (most-common-word "a." '()) "a"))
(cl-assert (equal (most-common-word "a, a, a, a, b,b,b,c, c" '("a")) "b"))
(cl-assert (equal (most-common-word "Apple? Orange! Banana; Apple: Melon, Grape," '("melon" "grape")) "apple")))

(check-most-common-word)