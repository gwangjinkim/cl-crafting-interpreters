(in-package :cl-lox-tw-tests)

(in-suite :cl-lox-tw-suite)

(test class-basic-instantiation
      (let* ((source "
    class Foo {}
    var foo = Foo();
    print foo;
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "Foo instance" output))))

(test class-properties
      (let* ((source "
    class Foo {}
    var foo = Foo();
    foo.bar = \"baz\";
    print foo.bar;
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "baz" output))))

(test class-methods
      (let* ((source "
    class Bacon {
      eat() {
        print \"Crunch crunch crunch!\";
      }
    }
    Bacon().eat();
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "Crunch crunch crunch!" output))))

(test class-this-keyword
      (let* ((source "
    class Person {
      sayName() {
        print this.name;
      }
    }
    var jane = Person();
    jane.name = \"Jane\";
    jane.sayName();
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "Jane" output))))

(test class-init-constructor
      (let* ((source "
    class Person {
      init(name) {
        this.name = name;
      }
      sayName() {
        print \"Hi, I am \" + this.name;
      }
    }
    var bill = Person(\"Bill\");
    bill.sayName();
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "Hi, I am Bill" output))))
