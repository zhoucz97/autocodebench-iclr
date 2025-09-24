(require 'cl-lib)

(defun swap-binary-halves (number)
  "Swap the first 16 bits (high bits) with the last 16 bits (low bits) of a 32-bit integer.
The input NUMBER is treated as a 32-bit unsigned integer, with leading zeros if necessary.
Returns the new number after swapping the halves."
  (let ((high-bits (ash (logand number #xFFFF0000) -16))  ; Extract high 16 bits and shift right
        (low-bits (logand number #x0000FFFF)))           ; Extract low 16 bits
    (logior (ash low-bits 16) high-bits)))               ; Shift low bits left and combine with high bits


(defun test-swap-binary-halves ()
(cl-assert (equal (swap-binary-halves 1314520) 249036820))
(cl-assert (equal (swap-binary-halves 65535) 4294901760))
(cl-assert (equal (swap-binary-halves 4294901760) 65535))
(cl-assert (equal (swap-binary-halves 0) 0)))

(test-swap-binary-halves)