(in-package :cl-lox-treewalk)

;; The parser maintains state via these variables during a parse run
(defvar *tokens* nil)
(defvar *current-token-index* 0)

(define-condition lox-parse-error (error)
    ((token :initarg :token :reader lox-parse-error-token)
     (message :initarg :message :reader lox-parse-error-message))
  (:report (lambda (condition stream)
             (format stream "Parse Error at ~A: ~A"
               (if (eq (token-type (lox-parse-error-token condition)) :eof)
                   "end"
                   (format nil "'~A'" (token-lexeme (lox-parse-error-token condition))))
               (lox-parse-error-message condition)))))

(defun parse (tokens)
  "Parses a list of tokens into a list of AST statement nodes."
  (setf *tokens* tokens)
  (setf *current-token-index* 0)
  (let ((statements nil))
    (loop while (not (is-at-end-p))
          do (let ((stmt (lox-declaration)))
               (when stmt (push stmt statements))))
    (nreverse statements)))

;;; --- HELPER FUNCTIONS ---

(defun peek-token ()
  (nth *current-token-index* *tokens*))

(defun previous-token ()
  (nth (- *current-token-index* 1) *tokens*))

(defun is-at-end-p ()
  (eq (token-type (peek-token)) :eof))

(defun lox-check-type (type)
  (if (is-at-end-p)
      nil
      (eq (token-type (peek-token)) type)))

(defun advance-token ()
  (unless (is-at-end-p)
    (incf *current-token-index*))
  (previous-token))

(defun match-token (&rest types)
  (dolist (type types)
    (when (lox-check-type type)
          (advance-token)
          (return-from match-token t)))
  nil)

(defun consume (type message)
  (if (lox-check-type type)
      (advance-token)
      (error 'lox-parse-error :token (peek-token) :message message)))

(defun synchronize ()
  (loop while (not (is-at-end-p))
        do (when (eq (token-type (previous-token)) :semicolon)
                 (return))
          (case (token-type (peek-token))
            ((:class :fun :var :for :if :while :print :return) (return)))
          (advance-token)))


;;; --- RECURSIVE DESCENT PARSING ---

(defun lox-declaration ()
  (handler-case
      (cond
       ((match-token :class) (class-declaration))
       ((match-token :fun) (function-declaration "function"))
       ((match-token :var) (var-declaration))
       (t (statement)))
    (lox-parse-error (c)
                     (format *error-output* "~A~%" c)
                     (synchronize)
                     nil)))

(defun class-declaration ()
  (let ((name (consume :identifier "Expect class name.")))
    (consume :left-brace "Expect '{' before class body.")
    (let ((methods nil))
      (loop while (and (not (lox-check-type :right-brace)) (not (is-at-end-p)))
            do (push (function-declaration "method") methods))
      (consume :right-brace "Expect '}' after class body.")
      (make-instance 'class-stmt :name name :methods (nreverse methods)))))

(defun var-declaration ()
  (let ((name (consume :identifier "Expect variable name.")))
    (let ((initializer nil))
      (when (match-token :equal)
            (setf initializer (expression)))
      (consume :semicolon "Expect ';' after variable declaration.")
      (make-instance 'var-stmt :name name :initializer initializer))))

(defun function-declaration (kind)
  (let ((name (consume :identifier (format nil "Expect ~A name." kind))))
    (consume :left-paren (format nil "Expect '(' after ~A name." kind))
    (let ((parameters nil))
      (unless (lox-check-type :right-paren)
        (push (consume :identifier "Expect parameter name.") parameters)
        (loop while (match-token :comma)
              do (when (>= (length parameters) 255)
                       (error 'lox-parse-error :token (peek-token) :message "Can't have more than 255 parameters."))
                (push (consume :identifier "Expect parameter name.") parameters)))
      (consume :right-paren "Expect ')' after parameters.")
      (consume :left-brace (format nil "Expect '{' before ~A body." kind))
      (let ((body (block-statement)))
        (make-instance 'function-stmt :name name :params (nreverse parameters) :body body)))))

(defun statement ()
  (cond
   ((match-token :for) (for-statement))
   ((match-token :if) (if-statement))
   ((match-token :print) (print-statement))
   ((match-token :return) (return-statement))
   ((match-token :while) (while-statement))
   ((match-token :left-brace) (make-instance 'block-stmt :statements (block-statement)))
   (t (expression-statement))))

(defun if-statement ()
  (consume :left-paren "Expect '(' after 'if'.")
  (let ((condition (expression)))
    (consume :right-paren "Expect ')' after if condition.")
    (let ((then-branch (statement))
          (else-branch nil))
      (when (match-token :else)
            (setf else-branch (statement)))
      (make-instance 'if-stmt :condition condition :then-branch then-branch :else-branch else-branch))))

(defun while-statement ()
  (consume :left-paren "Expect '(' after 'while'.")
  (let ((condition (expression)))
    (consume :right-paren "Expect ')' after condition.")
    (let ((body (statement)))
      (make-instance 'while-stmt :condition condition :body body))))

(defun for-statement ()
  (consume :left-paren "Expect '(' after 'for'.")
  (let ((initializer (cond
                      ((match-token :semicolon) nil)
                      ((match-token :var) (var-declaration))
                      (t (expression-statement))))
        (condition nil)
        (increment nil)
        (body nil))

    (if (not (lox-check-type :semicolon))
        (setf condition (expression)))
    (consume :semicolon "Expect ';' after loop condition.")

    (if (not (lox-check-type :right-paren))
        (setf increment (expression)))
    (consume :right-paren "Expect ')' after for clauses.")

    (setf body (statement))

    ;; Desugar for loop to while loop
    (when increment
          (setf body (make-instance 'block-stmt
                       :statements (list body (make-instance 'expression-stmt :expression increment)))))

    (when (null condition)
          (setf condition (make-instance 'literal-expr :value t)))

    (setf body (make-instance 'while-stmt :condition condition :body body))

    (when initializer
          (setf body (make-instance 'block-stmt :statements (list initializer body))))

    body))

(defun print-statement ()
  (let ((value (expression)))
    (consume :semicolon "Expect ';' after value.")
    (make-instance 'print-stmt :expression value)))

(defun return-statement ()
  (let ((keyword (previous-token))
        (value nil))
    (unless (lox-check-type :semicolon)
      (setf value (expression)))
    (consume :semicolon "Expect ';' after return value.")
    (make-instance 'return-stmt :keyword keyword :value value)))

(defun expression-statement ()
  (let ((expr (expression)))
    (consume :semicolon "Expect ';' after expression.")
    (make-instance 'expression-stmt :expression expr)))

(defun block-statement ()
  (let ((statements nil))
    (loop while (and (not (lox-check-type :right-brace)) (not (is-at-end-p)))
          do (let ((decl (lox-declaration)))
               (when decl (push decl statements))))
    (consume :right-brace "Expect '}' after block.")
    (nreverse statements)))

(defun expression ()
  (assignment))

(defun assignment ()
  (let ((expr (logic-or)))
    (if (match-token :equal)
        (let ((equals (previous-token))
              (value (assignment)))
          (cond
           ((typep expr 'variable-expr)
             (make-instance 'assign-expr :name (variable-name expr) :value value))
           ((typep expr 'get-expr)
             (make-instance 'set-expr :object (get-object expr) :name (get-name expr) :value value))
           (t (error 'lox-parse-error :token equals :message "Invalid assignment target."))))
        expr)))

(defun logic-or ()
  (let ((expr (logic-and)))
    (loop while (match-token :or)
          do (let ((operator (previous-token))
                   (right (logic-and)))
               (setf expr (make-instance 'logical-expr :left expr :operator operator :right right))))
    expr))

(defun logic-and ()
  (let ((expr (equality)))
    (loop while (match-token :and)
          do (let ((operator (previous-token))
                   (right (equality)))
               (setf expr (make-instance 'logical-expr :left expr :operator operator :right right))))
    expr))


(defun equality ()
  (let ((expr (comparison)))
    (loop while (match-token :bang-equal :equal-equal)
          do (let ((operator (previous-token))
                   (right (comparison)))
               (setf expr (make-instance 'binary-expr :left expr :operator operator :right right))))
    expr))

(defun comparison ()
  (let ((expr (term)))
    (loop while (match-token :greater :greater-equal :less :less-equal)
          do (let ((operator (previous-token))
                   (right (term)))
               (setf expr (make-instance 'binary-expr :left expr :operator operator :right right))))
    expr))

(defun term ()
  (let ((expr (factor)))
    (loop while (match-token :minus :plus)
          do (let ((operator (previous-token))
                   (right (factor)))
               (setf expr (make-instance 'binary-expr :left expr :operator operator :right right))))
    expr))

(defun factor ()
  (let ((expr (unary)))
    (loop while (match-token :slash :star)
          do (let ((operator (previous-token))
                   (right (unary)))
               (setf expr (make-instance 'binary-expr :left expr :operator operator :right right))))
    expr))

(defun unary ()
  (if (match-token :bang :minus)
      (let ((operator (previous-token))
            (right (unary)))
        (make-instance 'unary-expr :operator operator :right right))
      (call)))

(defun call ()
  (let ((expr (primary)))
    (loop
     (cond
      ((match-token :left-paren)
        (setf expr (finish-call expr)))
      ((match-token :dot)
        (let ((name (consume :identifier "Expect property name after '.'.")))
          (setf expr (make-instance 'get-expr :object expr :name name))))
      (t (return))))
    expr))

(defun finish-call (callee)
  (let ((arguments nil))
    (unless (lox-check-type :right-paren)
      (push (expression) arguments)
      (loop while (match-token :comma)
            do (when (>= (length arguments) 255)
                     (error 'lox-parse-error :token (peek-token) :message "Can't have more than 255 arguments."))
              (push (expression) arguments)))
    (consume :right-paren "Expect ')' after arguments.")
    (make-instance 'call-expr :callee callee :paren (previous-token) :arguments (nreverse arguments))))

(defun primary ()
  (cond
   ((match-token :false) (make-instance 'literal-expr :value nil))
   ((match-token :true) (make-instance 'literal-expr :value t))
   ((match-token :nil) (make-instance 'literal-expr :value nil)) ; Represented as Lisp nil

   ((match-token :this) (make-instance 'this-expr :keyword (previous-token)))

   ((match-token :number :string)
     (make-instance 'literal-expr :value (token-literal (previous-token))))

   ((match-token :identifier)
     (make-instance 'variable-expr :name (previous-token)))

   ((match-token :left-paren)
     (let ((expr (expression)))
       (consume :right-paren "Expect ')' after expression.")
       (make-instance 'grouping-expr :expression expr)))

   (t (error 'lox-parse-error :token (peek-token) :message "Expect expression."))))
