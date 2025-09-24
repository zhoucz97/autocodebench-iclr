(require 'cl-lib)

(defun find-substring-index (haystack needle)
  "Find the first occurrence of NEEDLE in HAYSTACK.
Returns the zero-based index of the first occurrence, or -1 if not found."
  (let ((index (string-match (regexp-quote needle) haystack)))
    (if index
        index
      -1)))



(defun test-find-substring-index ()
(cl-assert (equal (find-substring-index "hello world" "world") 6))
(cl-assert (equal (find-substring-index "hello world" "hello") 0))
(cl-assert (equal (find-substring-index "hello world" "abc") -1))
(cl-assert (equal (find-substring-index "abcabcabc" "abc") 0))
(cl-assert (equal (find-substring-index "abcabcabc" "cab") 2))
(cl-assert (equal (find-substring-index "openai" "ai") 4))
(cl-assert (equal (find-substring-index "openai" "") 0)))

(test-find-substring-index)