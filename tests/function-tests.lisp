(in-package :cl-lox-tw-tests)

(in-suite :cl-lox-tw-suite)

(test function-basic
      (let* ((source "
    fun sayHi(first, last) {
      print \"Hi, \" + first + \" \" + last + \"!\";
    }
    sayHi(\"Dear\", \"Reader\");
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "Hi, Dear Reader!" output))))

(test function-return-value
      (let* ((source "
    fun add(a, b) {
      return a + b;
    }
    print add(4, 5);
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "9" output))))

(test function-recursion-fibonacci
      (let* ((source "
    fun fib(n) {
      if (n <= 1) return n;
      return fib(n - 2) + fib(n - 1);
    }
    print fib(6);
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        ;; Fibonacci sequence: 0, 1, 1, 2, 3, 5, 8
        ;; fib(6) = 8
        (is (search "8" output))))

(test function-closures
      (let* ((source "
    fun makeCounter() {
      var i = 0;
      fun count() {
        i = i + 1;
        print i;
      }
      return count;
    }

    var counter = makeCounter();
    counter();
    counter();
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "1" output))
        (is (search "2" output))))
