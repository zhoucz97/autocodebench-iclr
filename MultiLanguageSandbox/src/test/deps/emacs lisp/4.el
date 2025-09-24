(require 'cl-lib)

(defun find-common-favorites (andy-list doris-list)
  "Find common favorite restaurants with the minimum index sum.
ANDY-LIST is a list of strings representing Andy's favorite restaurants.
DORIS-LIST is a list of strings representing Doris's favorite restaurants.
Returns a list of restaurant names that are common favorites with the least index sum."
  (let ((common-restaurants nil)
        (min-sum most-positive-fixnum))
    ;; Create a hash table for Andy's restaurants with their indices
    (let ((andy-hash (make-hash-table :test 'equal)))
      (dotimes (i (length andy-list))
        (puthash (nth i andy-list) i andy-hash)))
    
    ;; Iterate through Doris's list to find common restaurants
    (dotimes (i (length doris-list))
      (let* ((restaurant (nth i doris-list))
             (andy-index (gethash restaurant andy-hash)))
        (when andy-index
          (let ((sum (+ andy-index i)))
            (cond
             ((< sum min-sum)
              (setq min-sum sum)
              (setq common-restaurants (list restaurant)))
             ((= sum min-sum)
              (push restaurant common-restaurants)))))))
    
    ;; Return the result, sorted if there are multiple
    (sort common-restaurants 'string<)))


(defun check-find-common-favorites ()
(cl-assert (equal (find-common-favorites '("Shogun" "Tapioca Express" "Burger King" "KFC")
'("Piatti" "The Grill at Torrey Pines" "Hungry Hunter Steakhouse" "Shogun"))
'("Shogun")))
(cl-assert (equal (find-common-favorites '("Shogun" "Tapioca Express" "Burger King" "KFC")
'("KFC" "Shogun" "Burger King"))
'("Shogun")))
(cl-assert (equal (find-common-favorites '("Shogun" "Tapioca Express" "Burger King" "KFC")
'("KFC" "Burger King" "Tapioca Express" "Shogun"))
'("KFC" "Burger King" "Tapioca Express" "Shogun" )))
(cl-assert (equal (find-common-favorites '("KFC" "Shogun" "Burger King")
'("Piatti" "The Grill at Torrey Pines" "Hungry Hunter Steakhouse" "Shogun"))
'("Shogun"))))

(check-find-common-favorites)