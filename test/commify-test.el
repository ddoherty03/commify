;;; Commentary:

;; This file provides tests for commify.  To run the tests, first run
;;
;; M-x eval-buffer
;;
;; in this buffer, then,
;;
;; M-x ert (entering t as the argument)
;;
;; The t argument is for test selectionm and will run all of the tests.  If
;; any tests are added, changed, or deleted, re-run the eval-buffer command.

;;; Code:
(load-file "test-helper.el")

;; Adding the group-char
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
    (should (equal "42_34234_23483_14159" (commify--commas "42342342348314159" "_")))))

(ert-deftest commify-test-decimal-group-size-one ()
  "Test rendering of decimal number with group of 1"
  (should (equal "4_2_3_4_2_3_4_2_3_4_8_3_1_4_1_5_9"
                 (commify--commas "42342342348314159" "_" 1)))
  (should (equal "9_8_7" (commify--commas "987" "_" 1)))
  (should (equal "9" (commify--commas "9" "_" 1))))

(ert-deftest commify-test-decimal-group-size-small ()
  "Test rendering of decimal number with group size less than 1"
  (should (equal "42342342348314159" (commify--commas "42342342348314159" "_" 0)))
  (should (equal "42342342348314159" (commify--commas "42342342348314159" "_" -1)))
  (should (equal "987" (commify--commas "987" "_" 0))))

;;; Stripping away the group char

(ert-deftest commify-uncommify-test-decimal ()
  "Test removal of default group-char"
  (should (equal "8314159" (commify--uncommas "8,314,159")))
  (should (equal "987" (commify--uncommas "987"))))

(ert-deftest commify-uncommify-test-decimal-group-char ()
  "Test removal of non-default group char"
  (let ((commify-group-char "_"))
    (should (equal "8314159" (commify--uncommas "8_314_159")))
    (should (equal "987" (commify--uncommas "987" "_")))))
