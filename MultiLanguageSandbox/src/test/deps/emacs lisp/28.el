(require 'cl-lib)

(defun calculate-electricity-bill (kwh)
  "Calculate the electricity bill based on tiered rates.
For the first 150 kWh: 0.4463 per kWh
For 151-400 kWh: 0.4663 per kWh
Above 400 kWh: 0.5663 per kWh"
  (cond
   ((<= kwh 150) (* kwh 0.4463))
   ((<= kwh 400) (+ (* 150 0.4463) (* (- kwh 150) 0.4663)))
   (t (+ (* 150 0.4463) (* 250 0.4663) (* (- kwh 400) 0.5663)))))


(defun test-calculate-electricity-bill ()
(cl-assert (equal (calculate-electricity-bill 267) "121.5"))
(cl-assert (equal (calculate-electricity-bill 150) "66.9"))
(cl-assert (equal (calculate-electricity-bill 400) "183.5"))
)

(test-calculate-electricity-bill)