(asdf:defsystem #:cl-lox-vm
  :description "Bytecode Virtual Machine for Lox"
  :author "Lisp Interpreter Architect"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :components ((:module "src/vm"
                        :serial t
                        :components ((:file "packages")
                                     (:file "value")
                                     (:file "chunk")
                                     (:file "debug"))))
  :in-order-to ((test-op (test-op "cl-lox-vm/tests"))))

(asdf:defsystem #:cl-lox-vm/tests
  :depends-on (#:cl-lox-vm #:fiveam)
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "vm-chunk-tests"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :cl-lox-vm-suite)))
