(in-package :asdf-user)

(defsystem "cl-lox-treewalk"
  :description "Tree-walk interpreter for Lox (jlox equivalent)"
  :version "0.1.0"
  :author "Gwangjin Kim"
  :depends-on ()
  :components ((:module "src/treewalk"
                        :serial t
                        :components
                        ((:file "packages")
                         (:file "token")
                         (:file "scanner")
                         (:file "ast")
                         (:file "parser")
                         (:file "environment")
                         (:file "evaluator")
                         (:file "main"))))
  :in-order-to ((test-op (test-op "cl-lox-treewalk/tests"))))

(defsystem "cl-lox-treewalk/tests"
  :depends-on ("cl-lox-treewalk" "fiveam")
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "main")
                         (:file "scanner-tests")
                         (:file "parser-tests")
                         (:file "environment-tests")
                         (:file "evaluator-tests")
                         (:file "control-flow-tests")
                         (:file "function-tests"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :cl-lox-tw-suite)))
