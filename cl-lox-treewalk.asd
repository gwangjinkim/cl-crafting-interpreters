(in-package :asdf-user)

(defsystem "cl-lox-treewalk"
  :description "Tree-walk interpreter for Lox (jlox equivalent)"
  :version "0.1.0"
  :author "Gwangjin Kim"
  :depends-on ()
  :components ((:module "src/treewalk"
                        :components
                        ((:file "packages")
                         (:file "token" :depends-on ("packages"))
                         (:file "scanner" :depends-on ("token"))
                         (:file "ast" :depends-on ("packages"))
                         (:file "parser" :depends-on ("scanner" "ast"))
                         (:file "evaluator" :depends-on ("ast"))
                         (:file "main" :depends-on ("parser")))))
  :in-order-to ((test-op (test-op "cl-lox-treewalk/tests"))))

(defsystem "cl-lox-treewalk/tests"
  :depends-on ("cl-lox-treewalk" "fiveam")
  :components ((:module "tests"
                        :components
                        ((:file "main")
                         (:file "scanner-tests" :depends-on ("main"))
                         (:file "parser-tests" :depends-on ("scanner-tests"))
                         (:file "evaluator-tests" :depends-on ("parser-tests")))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :cl-lox-tw-suite)))
