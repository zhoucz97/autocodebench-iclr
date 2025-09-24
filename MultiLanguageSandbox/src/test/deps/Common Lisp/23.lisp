(defun count-reachable-apples (apple-heights fengfeng-height)
  "Counts the number of apples that Fengfeng can reach.
   'apple-heights' is a list of integers representing the heights of apples.
   'fengfeng-height' is an integer representing Fengfeng's maximum reach height.
   Returns the count of apples at or below Fengfeng's maximum height."
  (count-if (lambda (height) (<= height fengfeng-height)) apple-heights))


(defun test-count-reachable-apples ()
(assert (equal (count-reachable-apples '(120 130 140 150 160 170 180 190 200 210) 150) 4))
(assert (equal (count-reachable-apples '(100 101 102 103 104 105 106 107 108 109) 110) 10))
(assert (equal (count-reachable-apples '(110 115 120 125 130 135 140 145 150 155) 120) 3))
(assert (equal (count-reachable-apples '(200 199 198 197 196 195 194 193 192 191) 190) 0))
(assert (equal (count-reachable-apples '(101 102 103 104 105 106 107 108 109 110) 105) 5)))

(test-count-reachable-apples)