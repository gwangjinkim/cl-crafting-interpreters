(in-package :cl-lox-treewalk)

;; Base class for all AST nodes
(defclass expr () ())

;; Expressions
(defclass binary-expr (expr)
    ((left :initarg :left :accessor binary-left)
     (operator :initarg :operator :accessor binary-operator)
     (right :initarg :right :accessor binary-right)))

(defclass grouping-expr (expr)
    ((expression :initarg :expression :accessor grouping-expression)))

(defclass literal-expr (expr)
    ((value :initarg :value :accessor literal-value)))

(defclass unary-expr (expr)
    ((operator :initarg :operator :accessor unary-operator)
     (right :initarg :right :accessor unary-right)))

(defclass variable-expr (expr)
    ((name :initarg :name :accessor variable-name)))

(defclass assign-expr (expr)
    ((name :initarg :name :accessor assign-name)
     (value :initarg :value :accessor assign-value)))


;; Represents a logical operator expression (e.g. `and`, `or`)
(defclass logical-expr (expr)
    ((left :initarg :left :accessor logical-left)
     (operator :initarg :operator :accessor logical-operator)
     (right :initarg :right :accessor logical-right)))

;; Represents a function call expression (e.g. `foo(1, 2)`)
(defclass call-expr (expr)
    ((callee :initarg :callee :accessor call-callee)
     (paren :initarg :paren :accessor call-paren)
     (arguments :initarg :arguments :accessor call-arguments)))

(defclass get-expr (expr)
    ((object :initarg :object :accessor get-object)
     (name :initarg :name :accessor get-name)))

(defclass set-expr (expr)
    ((object :initarg :object :accessor set-object)
     (name :initarg :name :accessor set-name)
     (value :initarg :value :accessor set-value)))

(defclass this-expr (expr)
    ((keyword :initarg :keyword :accessor this-keyword)))

;; Base class for statements
(defclass stmt () ())

(defclass expression-stmt (stmt)
    ((expression :initarg :expression :accessor stmt-expression)))

(defclass print-stmt (stmt)
    ((expression :initarg :expression :accessor stmt-print-expr)))

(defclass var-stmt (stmt)
    ((name :initarg :name :accessor stmt-var-name)
     (initializer :initarg :initializer :accessor stmt-var-initializer)))

;; Represents a block of statements {...}
(defclass block-stmt (stmt)
    ((statements :initarg :statements :accessor stmt-block-statements)))

;; Represents an if/else conditional statement
(defclass if-stmt (stmt)
    ((condition :initarg :condition :accessor stmt-if-condition)
     (then-branch :initarg :then-branch :accessor stmt-if-then)
     (else-branch :initarg :else-branch :accessor stmt-if-else
                  :initform nil)))

;; Represents a while loop
(defclass while-stmt (stmt)
    ((condition :initarg :condition :accessor stmt-while-condition)
     (body :initarg :body :accessor stmt-while-body)))

;; Represents a function declaration
(defclass function-stmt (stmt)
    ((name :initarg :name :accessor stmt-function-name)
     (params :initarg :params :accessor stmt-function-params)
     (body :initarg :body :accessor stmt-function-body)))

(defclass class-stmt (stmt)
    ((name :initarg :name :accessor stmt-class-name)
     (methods :initarg :methods :accessor stmt-class-methods)))

;; Represents a return statement
(defclass return-stmt (stmt)
    ((keyword :initarg :keyword :accessor stmt-return-keyword)
     (value :initarg :value :accessor stmt-return-value
            :initform nil)))
