(defpackage :cl-lox-treewalk
  (:use :cl)
  (:export :main
           :scan-tokens
           :token-type
           :token-lexeme
           :token-literal
           :token-line
           ;; AST
           :expr :binary-expr :grouping-expr :literal-expr :unary-expr :variable-expr :assign-expr
           :stmt :expression-stmt :print-stmt :var-stmt :block-stmt
           :binary-left :binary-operator :binary-right
           :literal-value :unary-operator :unary-right
           :variable-name :assign-name :assign-value
           :stmt-expression :stmt-var-name :stmt-var-initializer
           :stmt-print-expr :stmt-block-statements
           :grouping-expression
           ;; Parser
           :parse :lox-parse-error :lox-parse-error-message
           ;; Evaluator
           :evaluate :execute :interpret))
