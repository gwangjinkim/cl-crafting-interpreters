(in-package :cl-lox-tw-tests)

(in-suite :cl-lox-tw-suite)

;; A helper macro to evaluate source code and return the Lisp value (mostly for expressions)
(defmacro evaluates-to (expected source)
  `(let* ((tokens (cl-lox-treewalk:scan-tokens ,source))
          (stmts (cl-lox-treewalk:parse tokens))
          (expr-stmt (first stmts)))
     (is (equal ,expected (cl-lox-treewalk:evaluate (cl-lox-treewalk:stmt-expression expr-stmt))))))

(test evaluate-literals
      (evaluates-to 123 "123;")
      (evaluates-to "lox" "\"lox\";")
      (evaluates-to t "true;")
      (evaluates-to nil "false;")
      (evaluates-to nil "nil;"))

(test evaluate-unary
      (evaluates-to -123 "-123;")
      (evaluates-to nil "!true;")
      (evaluates-to t "!false;")
      ;; nil is falsy, therefore !nil is true
      (evaluates-to t "!nil;")
      (evaluates-to nil "!123;"))

(test evaluate-binary-math
      (evaluates-to 10 "4 + 6;")
      (evaluates-to -2 "4 - 6;")
      (evaluates-to 24 "4 * 6;")
      (evaluates-to 3 "12 / 4;")
      (evaluates-to 15 "3 + 4 * 3;") ; Precedence check
      (evaluates-to 21 "(3 + 4) * 3;")) ; Grouping check

(test evaluate-binary-string
      (evaluates-to "helloworld" "\"hello\" + \"world\";"))

(test evaluate-binary-logic
      (evaluates-to t "4 < 6;")
      (evaluates-to nil "4 > 6;")
      (evaluates-to t "6 >= 6;")
      (evaluates-to nil "6 <= 4;")
      (evaluates-to t "6 == 6;")
      (evaluates-to nil "6 != 6;")
      (evaluates-to t "\"lox\" == \"lox\";")
      (evaluates-to t "nil == nil;"))

(test evaluate-runtime-errors
      ;; Subtraction with a string must dispatch to our runtime-error function and ultimately throw
      (signals error
               (let* ((tokens (cl-lox-treewalk:scan-tokens "4 - \"hello\";"))
                      (stmts (cl-lox-treewalk:parse tokens)))
                 (cl-lox-treewalk:evaluate (cl-lox-treewalk:stmt-expression (first stmts)))))
      (signals error
               (let* ((tokens (cl-lox-treewalk:scan-tokens "10 / 0;"))
                      (stmts (cl-lox-treewalk:parse tokens)))
                 (cl-lox-treewalk:evaluate (cl-lox-treewalk:stmt-expression (first stmts))))))

(test evaluate-print-statement
      (let* ((source "print 1 + 2 * 3;
                  print \"lox\";
                  print nil;")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens))
             ;; Capture standard output to verify `execute` prints things correctly
             (output (with-output-to-string (*standard-output*)
                       (cl-lox-treewalk:interpret stmts))))
        (is (search "7" output))
        (is (search "lox" output))
        (is (search "nil" output))))
