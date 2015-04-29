(load-library "commands")

(ert-deftest camdez/cycle-test ()
  (should (eq (camdez/cycle '(1 2 3) 2) 3))
  (should (eq (camdez/cycle '(1 2 3) 3) 1))
  (should (eq (camdez/cycle '(1 2 3) 4) 1))
  (should (eq (camdez/cycle '(t nil) t) nil))
  (should (eq (camdez/cycle '(t nil) nil) t)))
