(defun parse-date (date-str)
  (let* ((month-names '("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
                        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
         (parts (uiop:split-string date-str :separator " "))
         (day (parse-integer (first parts)))
         (month-name (second parts))
         (year (parse-integer (third parts)))
         (month (position month-name month-names :test #'string=)))
    (list day (1+ month) year)))


(defun check-parse-date ()
;; Test the 'parse-date' function with various date strings.
(assert (equal (parse-date "16 Aug 1980") '(16 8 1980)))
(assert (equal (parse-date "1 Jan 2023") '(1 1 2023)))
(assert (equal (parse-date "25 Dec 1999") '(25 12 1999)))
(assert (equal (parse-date "31 Oct 2025") '(31 10 2025)))
(assert (equal (parse-date "20 Feb 2010") '(20 2 2010))))

(check-parse-date)