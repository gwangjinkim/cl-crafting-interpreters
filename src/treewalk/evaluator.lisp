(in-package :cl-lox-treewalk)

;; The global environment used by the interpreter
(defvar *global-environment* (make-environment))
(defvar *environment* *global-environment*)

(defclass lox-callable ()
    ((arity :initarg :arity :reader callable-arity)
     (call-func :initarg :call-func :reader callable-func)))

(defmethod arity ((callable lox-callable))
  (callable-arity callable))

(defmethod call-callable ((callable lox-callable) arguments)
  (funcall (callable-func callable) arguments))

(define-variable *global-environment* "clock"
                 (make-instance 'lox-callable
                   :arity 0
                   :call-func (lambda (args)
                                (declare (ignore args))
                                ;; Return current time in seconds as float
                                (float (/ (get-internal-real-time) internal-time-units-per-second)))))

;; The evaluate generic function will dispatch dynamically based on the AST node type.
(defgeneric evaluate (node)
  (:documentation "Evaluates an AST node and returns a Lox runtime value."))

(defmethod evaluate ((node literal-expr))
  (literal-value node))

(defmethod evaluate ((node grouping-expr))
  (evaluate (grouping-expression node)))

(defmethod evaluate ((node variable-expr))
  (get-variable *environment* (token-lexeme (variable-name node)) (variable-name node)))

(defmethod evaluate ((node this-expr))
  (get-variable *environment* "this" (this-keyword node)))

(defmethod evaluate ((node assign-expr))
  (let ((value (evaluate (assign-value node))))
    (assign-variable *environment* (token-lexeme (assign-name node)) value (assign-name node))
    value))

(defmethod evaluate ((node call-expr))
  (let ((callee (evaluate (call-callee node)))
        (arguments (mapcar #'evaluate (call-arguments node))))
    (unless (typep callee 'lox-callable)
      (runtime-error (call-paren node) "Can only call functions and classes."))
    (unless (= (length arguments) (arity callee))
      (runtime-error (call-paren node) (format nil "Expected ~A arguments but got ~A."
                                         (arity callee) (length arguments))))
    (call-callable callee arguments)))

(defmethod evaluate ((node get-expr))
  (let ((object (evaluate (get-object node))))
    (if (typep object 'lox-instance)
        (get-property object (get-name node))
        (runtime-error (get-name node) "Only instances have properties."))))

(defmethod evaluate ((node set-expr))
  (let ((object (evaluate (set-object node))))
    (if (typep object 'lox-instance)
        (let ((value (evaluate (set-value node))))
          (set-property object (set-name node) value)
          value)
        (runtime-error (set-name node) "Only instances have fields."))))

(defmethod evaluate ((node logical-expr))
  (let ((left (evaluate (logical-left node)))
        (operator (logical-operator node)))
    (if (eq (token-type operator) :or)
        (if (is-truthy left) (return-from evaluate left))
        (if (not (is-truthy left)) (return-from evaluate left)))
    (evaluate (logical-right node))))

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

(defmethod execute ((stmt var-stmt))
  (let ((value nil))
    (when (stmt-var-initializer stmt)
          (setf value (evaluate (stmt-var-initializer stmt))))
    (define-variable *environment* (token-lexeme (stmt-var-name stmt)) value))
  nil)

(defmethod execute ((stmt if-stmt))
  (if (is-truthy (evaluate (stmt-if-condition stmt)))
      (execute (stmt-if-then stmt))
      (when (stmt-if-else stmt)
            (execute (stmt-if-else stmt))))
  nil)

(defmethod execute ((stmt while-stmt))
  (loop while (is-truthy (evaluate (stmt-while-condition stmt)))
        do (execute (stmt-while-body stmt)))
  nil)

(defun execute-block (statements env)
  (let ((previous *environment*))
    (unwind-protect
        (progn
         (setf *environment* env)
         (dolist (stmt statements)
           (execute stmt)))
      (setf *environment* previous))))

(defmethod execute ((stmt block-stmt))
  (execute-block (stmt-block-statements stmt) (make-environment *environment*))
  nil)

(defclass lox-function (lox-callable)
    ((declaration :initarg :declaration :reader lox-function-declaration)
     (closure :initarg :closure :reader lox-function-closure)))

(defmethod arity ((func lox-function))
  (length (stmt-function-params (lox-function-declaration func))))

(defmethod call-callable ((func lox-function) arguments)
  (let ((environment (make-environment (lox-function-closure func))))
    (loop for param in (stmt-function-params (lox-function-declaration func))
          for arg in arguments
          do (define-variable environment (token-lexeme param) arg))
    (catch 'lox-return
      (execute-block (stmt-function-body (lox-function-declaration func)) environment)
      nil)))

(defmethod execute ((stmt function-stmt))
  (let ((function (make-instance 'lox-function
                    :declaration stmt
                    :closure *environment*)))
    (define-variable *environment* (token-lexeme (stmt-function-name stmt)) function))
  nil)

(defmethod execute ((stmt return-stmt))
  (let ((value nil))
    (when (stmt-return-value stmt)
          (setf value (evaluate (stmt-return-value stmt))))
    (throw 'lox-return value)))

(defclass lox-class (lox-callable)
    ((name :initarg :name :reader lox-class-name)
     (methods :initarg :methods :reader lox-class-methods)))

(defmethod print-object ((class lox-class) stream)
  (format stream "<class ~A>" (lox-class-name class)))

(defmethod arity ((class lox-class))
  (let ((initializer (gethash "init" (lox-class-methods class))))
    (if initializer
        (arity initializer)
        0)))

(defmethod call-callable ((class lox-class) arguments)
  (let ((instance (make-instance 'lox-instance :class class)))
    (let ((initializer (gethash "init" (lox-class-methods class))))
      (when initializer
            (call-callable (bind-method initializer instance) arguments)))
    instance))

(defclass lox-instance ()
    ((class :initarg :class :reader lox-instance-class)
     (fields :initform (make-hash-table :test 'equal) :reader lox-instance-fields)))

(defmethod print-object ((instance lox-instance) stream)
  (format stream "<~A instance>" (lox-class-name (lox-instance-class instance))))

(defun get-property (instance name)
  (multiple-value-bind (val present) (gethash (token-lexeme name) (lox-instance-fields instance))
    (if present
        val
        (let ((method (gethash (token-lexeme name) (lox-class-methods (lox-instance-class instance)))))
          (if method
              (bind-method method instance)
              (runtime-error name (format nil "Undefined property '~A'." (token-lexeme name))))))))

(defun set-property (instance name value)
  (setf (gethash (token-lexeme name) (lox-instance-fields instance)) value))

(defun bind-method (method instance)
  (let ((environment (make-environment (lox-function-closure method))))
    (define-variable environment "this" instance)
    (make-instance 'lox-function
      :declaration (lox-function-declaration method)
      :closure environment)))

(defmethod execute ((stmt class-stmt))
  (define-variable *environment* (token-lexeme (stmt-class-name stmt)) nil)
  (let ((methods (make-hash-table :test 'equal)))
    (dolist (method-stmt (stmt-class-methods stmt))
      (let ((function (make-instance 'lox-function
                        :declaration method-stmt
                        :closure *environment*)))
        (setf (gethash (token-lexeme (stmt-function-name method-stmt)) methods) function)))
    (let ((klass (make-instance 'lox-class
                   :name (token-lexeme (stmt-class-name stmt))
                   :methods methods)))
      (assign-variable *environment* (token-lexeme (stmt-class-name stmt)) klass (stmt-class-name stmt))))
  nil)

(defun interpret (statements)
  "Main entry point for evaluating a list of AST statements."
  ;; For now, execute each statement sequentially. Error handling will be added.
  (handler-bind ((error (lambda (c)
                          (format *error-output* "~A~%" c)
                          (return-from interpret nil))))
    (dolist (stmt statements)
      (execute stmt))))
