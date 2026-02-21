(in-package :cl-lox-tw-tests)

(in-suite :cl-lox-tw-suite)

;; Simple test ensuring scanner outputs exact tokens
(test scan-simple-tokens
      (let* ((source "var a = 1 + 2;")
             (tokens (cl-lox-treewalk::scan-tokens source)))

        (is (= (length tokens) 8)) ; var, a, =, 1, +, 2, ;, EOF

        (is (eq (cl-lox-treewalk::token-type (nth 0 tokens)) :VAR)) ; var is a keyword
        (is (equal (cl-lox-treewalk::token-lexeme (nth 0 tokens)) "var"))

        (is (eq (cl-lox-treewalk::token-type (nth 1 tokens)) :IDENTIFIER))
        (is (equal (cl-lox-treewalk::token-lexeme (nth 1 tokens)) "a"))

        (is (eq (cl-lox-treewalk::token-type (nth 2 tokens)) :EQUAL))

        (is (eq (cl-lox-treewalk::token-type (nth 3 tokens)) :NUMBER))
        (is (equal (cl-lox-treewalk::token-literal (nth 3 tokens)) 1))

        (is (eq (cl-lox-treewalk::token-type (nth 4 tokens)) :PLUS))

        (is (eq (cl-lox-treewalk::token-type (nth 5 tokens)) :NUMBER))
        (is (equal (cl-lox-treewalk::token-literal (nth 5 tokens)) 2))

        (is (eq (cl-lox-treewalk::token-type (nth 6 tokens)) :SEMICOLON))))

(test scan-strings
      (let* ((source "\"hello world\"")
             (tokens (cl-lox-treewalk::scan-tokens source)))
        (is (= (length tokens) 2)) ; string, EOF
        (is (eq (cl-lox-treewalk::token-type (nth 0 tokens)) :STRING))
        (is (equal (cl-lox-treewalk::token-literal (nth 0 tokens)) "hello world"))))

(test scan-comments-and-whitespace
      (let* ((source "// this is a comment
                  var b = 2; // inline comment")
             (tokens (cl-lox-treewalk::scan-tokens source)))
        (is (= (length tokens) 6)) ; var, b, =, 2, ;, EOF (comments and whitespace ignored)
        (is (eq (cl-lox-treewalk::token-type (nth 0 tokens)) :VAR))
        (is (equal (cl-lox-treewalk::token-lexeme (nth 0 tokens)) "var"))))

(test scan-edge-cases
      (let* ((source "!= == <= >=")
             (tokens (cl-lox-treewalk::scan-tokens source)))
        (is (= (length tokens) 5))
        (is (eq (cl-lox-treewalk::token-type (nth 0 tokens)) :BANG-EQUAL))
        (is (eq (cl-lox-treewalk::token-type (nth 1 tokens)) :EQUAL-EQUAL))
        (is (eq (cl-lox-treewalk::token-type (nth 2 tokens)) :LESS-EQUAL))
        (is (eq (cl-lox-treewalk::token-type (nth 3 tokens)) :GREATER-EQUAL))))
