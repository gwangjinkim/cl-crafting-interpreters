(in-package :cl-lox-tw-tests)

(in-suite :cl-lox-tw-suite)

(test parse-simple-math
      (let* ((source "print 1 + 2 * 3;")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens)))

        (is (= (length stmts) 1))
        (let ((stmt (first stmts)))
          ;; Should be a print statement
          (is (typep stmt 'cl-lox-treewalk:print-stmt))

          (let ((expr (cl-lox-treewalk:stmt-print-expr stmt)))
            ;; The expression should be a binary expression (+)
            (is (typep expr 'cl-lox-treewalk:binary-expr))
            (is (eq (cl-lox-treewalk:token-type (cl-lox-treewalk:binary-operator expr)) :PLUS))

            ;; Left of + is 1
            (is (typep (cl-lox-treewalk:binary-left expr) 'cl-lox-treewalk:literal-expr))
            (is (= (cl-lox-treewalk:literal-value (cl-lox-treewalk:binary-left expr)) 1))

            ;; Right of + is a binary expression (*) due to precedence
            (let ((right (cl-lox-treewalk:binary-right expr)))
              (is (typep right 'cl-lox-treewalk:binary-expr))
              (is (eq (cl-lox-treewalk:token-type (cl-lox-treewalk:binary-operator right)) :STAR))

              (is (= (cl-lox-treewalk:literal-value (cl-lox-treewalk:binary-left right)) 2))
              (is (= (cl-lox-treewalk:literal-value (cl-lox-treewalk:binary-right right)) 3)))))))

(test parse-var-declaration
      (let* ((source "var language = \"lox\";")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens)))
        (is (= (length stmts) 1))
        (let ((stmt (first stmts)))
          (is (typep stmt 'cl-lox-treewalk:var-stmt))
          (is (equal (cl-lox-treewalk:token-lexeme (cl-lox-treewalk:stmt-var-name stmt)) "language"))
          (is (typep (cl-lox-treewalk:stmt-var-initializer stmt) 'cl-lox-treewalk:literal-expr))
          (is (equal (cl-lox-treewalk:literal-value (cl-lox-treewalk:stmt-var-initializer stmt)) "lox")))))

(test parse-error-recovery
      ;; Missing semicolon should error but synchronize and parse the second statement
      (let* ((source "var a = 1 var b = 2;")
             (tokens (cl-lox-treewalk:scan-tokens source))
             (stmts (cl-lox-treewalk:parse tokens)))
        ;; We expect synchronization to throw away the first broken statement, and parse the second
        (is (= (length stmts) 1))
        (let ((stmt (first stmts)))
          (is (typep stmt 'cl-lox-treewalk:var-stmt))
          (is (equal (cl-lox-treewalk:token-lexeme (cl-lox-treewalk:stmt-var-name stmt)) "b")))))
