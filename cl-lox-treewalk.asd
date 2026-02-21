(in-package :asdf-user)

(defsystem "cl-lox-treewalk"
  :description "Tree-walk interpreter for Lox (jlox equivalent)"
  :version "0.1.0"
  :author "Gwangjin Kim"
  :depends-on ()
  :components ((:module "src/treewalk"
                :components
                ((:file "main"))))
  :in-order-to ((test-op (test-op "cl-lox-treewalk/tests"))))

(defsystem "cl-lox-treewalk/tests"
  :depends-on ("cl-lox-treewalk" "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :cl-lox-tw-suite)))
