(defun construct-preorder (inorder postorder)
  (if (or (null inorder) (null postorder))
      ""
      (let* ((root (car (last postorder)))
             (root-pos (position root inorder))
             (left-in (subseq inorder 0 root-pos))
             (right-in (subseq inorder (1+ root-pos)))
             (left-post (subseq postorder 0 (length left-in)))
             (right-post (subseq postorder (length left-in) (1- (length postorder)))))
        (concatenate 'string
                     (string root)
                     (construct-preorder left-in left-post)
                     (construct-preorder right-in right-post)))))


(defun test-construct-preorder ()
(assert (string= (construct-preorder "DBEAC" "DEBCA") "ABDEC"))
(assert (string= (construct-preorder "HGFEIDBA" "HGFIEDBA") "ABDEFGHI"))
(assert (string= (construct-preorder "BADC" "BDCA") "ABCD"))
(assert (string= (construct-preorder "FBAEDC" "FBEADC") "CDABFE"))
(assert (string= (construct-preorder "A" "A") "A")))

(test-construct-preorder)