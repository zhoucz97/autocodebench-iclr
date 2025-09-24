(require 'cl-lib)

(defun comet-group-match (comet-name group-name)
  "Determine if a GROUP-NAME should go with a COMET-NAME based on their names.
Both names are strings of uppercase letters. The function calculates the product
of letter values (A=1, B=2, ..., Z=26) for both names. If the mod 47 of both products
is equal, it returns \"GO\", else \"STAY\"."
  (let ((comet-product 1)
        (group-product 1))
    ;; Calculate product for comet name
    (dolist (char (string-to-list comet-name))
      (setq comet-product (* comet-product (- char ?A 1))))
    ;; Calculate product for group name
    (dolist (char (string-to-list group-name))
      (setq group-product (* group-product (- char ?A 1))))
    ;; Compare mod 47 of both products
    (if (= (mod comet-product 47) (mod group-product 47))
        "GO"
      "STAY")))


(defun test-comet-group-match ()
(cl-assert (equal (comet-group-match "COMETQ" "HVNGAT") "GO"))
(cl-assert (equal (comet-group-match "ABC" "XYZ") "STAY"))
(cl-assert (equal (comet-group-match "HALLEY" "TEAM") "STAY"))
(cl-assert (equal (comet-group-match "SWIFT" "ROCKY") "STAY"))
(cl-assert (equal (comet-group-match "ORION" "ORION") "GO"))
(cl-assert (equal (comet-group-match "VEGA" "NOVA") "STAY"))
(cl-assert (equal (comet-group-match "LYRA" "SIGMA") "STAY"))
(cl-assert (equal (comet-group-match "PULSAR" "QUARK") "STAY")))

(test-comet-group-match)