(defpackage :cl-lox-treewalk
  (:use :cl)
  (:export :main
           :scan-tokens
           :token-type
           :token-lexeme
           :token-literal
           :token-line
           ;; AST
           :expr :binary-expr :grouping-expr :literal-expr :unary-expr :variable-expr :assign-expr :logical-expr :call-expr
           :stmt :expression-stmt :print-stmt :var-stmt :block-stmt :if-stmt :while-stmt :function-stmt :return-stmt
           :binary-left :binary-operator :binary-right
           :literal-value :unary-operator :unary-right
           :variable-name :assign-name :assign-value
           :logical-left :logical-operator :logical-right
           :call-callee :call-paren :call-arguments
           :stmt-expression :stmt-var-name :stmt-var-initializer
           :stmt-print-expr :stmt-block-statements
           :stmt-if-condition :stmt-if-then :stmt-if-else
           :stmt-while-condition :stmt-while-body
           :stmt-function-name :stmt-function-params :stmt-function-body
           :stmt-return-keyword :stmt-return-value
           :grouping-expression
           ;; Parser
           :parse :lox-parse-error :lox-parse-error-message
           ;; Evaluator
           :evaluate :execute :interpret
           ;; Environment
           :environment :make-environment :define-variable :get-variable :assign-variable))
