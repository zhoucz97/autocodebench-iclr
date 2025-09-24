(require 'cl-lib)

(defun calculate-scholarships (students)
  "Calculate the total scholarship amount for each student in STUDENTS.
Each student is represented as a list containing:
(name final-score class-score student-leader? western-province? papers-published)
Returns a list of (name total-scholarship) pairs."
  (mapcar
   (lambda (student)
     (let* ((name (nth 0 student))
            (final-score (nth 1 student))
            (class-score (nth 2 student))
            (student-leader (nth 3 student))
            (western-province (nth 4 student))
            (papers-published (nth 5 student))
            
            ;; Calculate base scholarship based on final score
            (base-scholarship
             (cond
              ((>= final-score 90) 8000)
              ((>= final-score 80) 6000)
              ((>= final-score 70) 4000)
              (t 2000)))
            
            ;; Calculate additional scholarships
            (leader-bonus (if (string= student-leader "Y") 1000 0))
            (western-bonus (if (string= western-province "Y") 2000 0))
            (paper-bonus (* papers-published 1000))
            
            ;; Total scholarship
            (total (+ base-scholarship leader-bonus western-bonus paper-bonus)))
       
       (list name total)))
   students))


(defun test-calculate-scholarships ()
(let ((student-data '(("YaoLin" 87 82 "Y" "N" 0) ("ChenRuiyi" 88 78 "N" "Y" 1) ("LiXin" 92 88 "N" "N" 0) ("ZhangQin" 83 87 "Y" "N" 1))))
(cl-assert (equal (calculate-scholarships student-data)'("ChenRuiyi" 9000 28700))))
)
(test-calculate-scholarships)