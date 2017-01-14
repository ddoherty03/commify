(ert-deftest commify-test-decimal ()
  "Test rendering of ordinary decimal number with defaults"
  (should (equal "8,314,159" (commify--commas "8314159")))
  (should (equal "987" (commify--commas "987"))))

(ert-deftest commify-test-decimal-group-char ()
  "Test rendering of decimal number with group char"
  (should (equal "8_314_159" (commify--commas "8314159" "_")))
  (should (equal "987" (commify--commas "987" "_")))
  (let ((commify-group-char "_"))
    (should (equal "8_314_159" (commify--commas "8314159")))
    (should (equal "987" (commify--commas "987" "_")))))

(ert-deftest commify-test-decimal-group-size ()
  "Test rendering of decimal number with group char"
  (should (equal "831_4159" (commify--commas "8314159" "_" 4)))
  (should (equal "4_23_42_34_23_48_31_41_59" (commify--commas "42342342348314159" "_" 2)))
  (should (equal "42_34234_23483_14159" (commify--commas "42342342348314159" "_" 5)))
  (should (equal "987" (commify--commas "987" "_" 4)))
  (let ((commify-group-size 5))
    (should (equal "42_34234_23483_14159" (commify--commas "42342342348314159" "_")))
  ))

(ert-deftest commify-test-decimal-group-size-small ()
  "Test rendering of decimal number with group size less than 1"
  (should (equal "42342342348314159" (commify--commas "42342342348314159" "_" 0)))
  (should (equal "42342342348314159" (commify--commas "42342342348314159" "_" -1)))
  (should (equal "987" (commify--commas "987" "_" 0))))
