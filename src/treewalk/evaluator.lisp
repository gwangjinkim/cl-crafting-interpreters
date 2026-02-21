(in-package :cl-lox-treewalk)

;; The evaluate generic function will dispatch dynamically based on the AST node type.
(defgeneric evaluate (node)
  (:documentation "Evaluates an AST node and returns a Lox runtime value."))

(defmethod evaluate ((node literal-expr))
  (literal-value node))

(defmethod evaluate ((node grouping-expr))
  (evaluate (grouping-expression node)))

;; Helper functions for Lox runtime truthiness and equality
(defun is-truthy (value)
  (cond
   ((null value) nil) ; Lox nil -> Lisp nil, which is falsey
   ((eq value :false) nil) ; Just in case we pass an explicit keyword
   (t t))) ; Everything else is true in Lox

(defun is-equal (a b)
  ;; For numbers and strings, Lisp's EQUAL is sufficient and handles types.
  ;; For nil, (EQUAL nil nil) applies.
  (equal a b))

(defvar *runtime-error-handler* nil
        "Dynamically bound function to handle Lox runtime errors.")

(defun runtime-error (token message)
  (if *runtime-error-handler*
      (funcall *runtime-error-handler* token message)
      (error "Runtime Error: [line ~A] ~A" (token-line token) message)))

(defun check-number-operand (operator operand)
  (unless (numberp operand)
    (runtime-error operator "Operand must be a number.")))

(defun check-number-operands (operator left right)
  (unless (and (numberp left) (numberp right))
    (runtime-error operator "Operands must be numbers.")))

(defmethod evaluate ((node unary-expr))
  (let ((right (evaluate (unary-right node)))
        (operator (unary-operator node)))
    (case (token-type operator)
      (:minus
       (check-number-operand operator right)
       (- right))
      (:bang
       (not (is-truthy right)))
      (otherwise nil))))

(defmethod evaluate ((node binary-expr))
  (let ((left (evaluate (binary-left node)))
        (right (evaluate (binary-right node)))
        (operator (binary-operator node)))
    (case (token-type operator)
      (:minus
       (check-number-operands operator left right)
       (- left right))
      (:slash
       (check-number-operands operator left right)
       (if (zerop right)
           (runtime-error operator "Division by zero.")
           (/ left right)))
      (:star
       (check-number-operands operator left right)
       (* left right))
      (:plus
       ;; Lox + operator is overloaded for numbers and strings
       (cond
        ((and (numberp left) (numberp right))
          (+ left right))
        ((and (stringp left) (stringp right))
          (concatenate 'string left right))
        (t
          (runtime-error operator "Operands must be two numbers or two strings."))))
      (:greater
       (check-number-operands operator left right)
       (> left right))
      (:greater-equal
       (check-number-operands operator left right)
       (>= left right))
      (:less
       (check-number-operands operator left right)
       (< left right))
      (:less-equal
       (check-number-operands operator left right)
       (<= left right))
      (:bang-equal
       (not (is-equal left right)))
      (:equal-equal
       (is-equal left right))
      (otherwise nil))))

;; Statement Execution
(defgeneric execute (stmt)
  (:documentation "Executes a Lox statement."))

(defmethod execute ((stmt expression-stmt))
  (evaluate (stmt-expression stmt))
  nil)

(defmethod execute ((stmt print-stmt))
  (let ((value (evaluate (stmt-print-expr stmt))))
    ;; Lox prints nil as "nil", numbers nicely
    (format t "~A~%" (if (null value) "nil" value))
    nil))

(defun interpret (statements)
  "Main entry point for evaluating a list of AST statements."
  ;; For now, execute each statement sequentially. Error handling will be added.
  (handler-bind ((error (lambda (c)
                          (format *error-output* "~A~%" c)
                          (return-from interpret nil))))
    (dolist (stmt statements)
      (execute stmt))))
