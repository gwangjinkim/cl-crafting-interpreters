(defpackage :cl-lox-tw-tests
  (:use :cl :fiveam))

(in-package :cl-lox-tw-tests)

(def-suite :cl-lox-tw-suite
           :description "Test suite for the Tree-walk Interpreter.")

(in-suite :cl-lox-tw-suite)

(test dummy-test
      "A simple assertion to verify the test harness is running."
      (is (= 1 1)))
