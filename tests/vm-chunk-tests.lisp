(in-package :cl-user)
(defpackage #:cl-lox-vm-tests
  (:use #:cl #:fiveam #:cl-lox-vm)
  (:export #:run-tests))
(in-package #:cl-lox-vm-tests)

(def-suite :cl-lox-vm-suite
           :description "Test suite for the cl-lox-vm Bytecode Virtual Machine.")
(in-suite :cl-lox-vm-suite)

(test chunk-basic-write
      (let ((chunk (cl-lox-vm:make-chunk)))
        (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-return+ 123)
        (is (= 1 (cl-lox-vm::chunk-count chunk)))
        (is (= cl-lox-vm:+op-return+ (aref (cl-lox-vm::chunk-code chunk) 0)))
        (is (= 123 (aref (cl-lox-vm::chunk-lines chunk) 0)))
        (cl-lox-vm::free-chunk chunk)
        (is (= 0 (cl-lox-vm::chunk-count chunk)))))

(test chunk-constants
      (let* ((chunk (cl-lox-vm:make-chunk))
             (index (cl-lox-vm:add-constant chunk 1.2d0)))
        (is (= 0 index))
        (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-constant+ 123)
        (cl-lox-vm:write-chunk chunk index 123)

        (let* ((constants (cl-lox-vm::chunk-constants chunk))
               (val (aref (cl-lox-vm::value-array-values constants) index)))
          (is (= 1.2d0 val)))
        (cl-lox-vm::free-chunk chunk)))
