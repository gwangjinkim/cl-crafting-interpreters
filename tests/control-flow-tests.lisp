(in-package :cl-lox-tw-tests)

(in-suite :cl-lox-tw-suite)

(test control-flow-if-else
      (let* ((source "
    var a = 1;
    if (true) {
      a = 2;
    } else {
      a = 3;
    }
    print a;

    var b = 1;
    if (false) {
      b = 2;
    } else {
      b = 3;
    }
    print b;
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "2" output))
        (is (search "3" output))))

(test control-flow-logical-operators
      (let* ((source "
    print \"hi\" or 2; 
    print nil or \"yes\";
    print nil and \"yes\";
    print \"hi\" and \"yes\";
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        ;; Lox 'or' returns first truthy value, or last falsy if all falsy
        (is (search "hi" output))
        (is (search "yes" output))
        ;; Lox 'and' returns first falsy value, or last truthy if all truthy
        (is (search "nil" output))))

(test control-flow-while-loop
      (let* ((source "
    var count = 0;
    while (count < 3) {
      print count;
      count = count + 1;
    }
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "0" output))
        (is (search "1" output))
        (is (search "2" output))))

(test control-flow-for-loop
      (let* ((source "
    var sum = 0;
    for (var i = 0; i < 5; i = i + 1) {
      sum = sum + i;
    }
    print sum;
  ")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             (output (with-output-to-string (*standard-output*)
                       (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                       (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                       (cl-lox-treewalk:interpret stmts))))
        ;; sum = 0 + 1 + 2 + 3 + 4 = 10
        (is (search "10" output))))
