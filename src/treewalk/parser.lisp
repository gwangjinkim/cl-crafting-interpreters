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
       ((match-token :var) (var-declaration))
       (t (statement)))
    (lox-parse-error (c)
                     (format *error-output* "~A~%" c)
                     (synchronize)
                     nil)))

(defun var-declaration ()
  (let ((name (consume :identifier "Expect variable name.")))
    (let ((initializer nil))
      (when (match-token :equal)
            (setf initializer (expression)))
      (consume :semicolon "Expect ';' after variable declaration.")
      (make-instance 'var-stmt :name name :initializer initializer))))

(defun statement ()
  (cond
   ((match-token :print) (print-statement))
   ((match-token :left-brace) (make-instance 'block-stmt :statements (block-statement)))
   (t (expression-statement))))

(defun print-statement ()
  (let ((value (expression)))
    (consume :semicolon "Expect ';' after value.")
    (make-instance 'print-stmt :expression value)))

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
  (let ((expr (equality)))
    (if (match-token :equal)
        (let ((equals (previous-token))
              (value (assignment)))
          (if (typep expr 'variable-expr)
              (make-instance 'assign-expr :name (variable-name expr) :value value)
              (error 'lox-parse-error :token equals :message "Invalid assignment target.")))
        expr)))

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
      (primary)))

(defun primary ()
  (cond
   ((match-token :false) (make-instance 'literal-expr :value nil))
   ((match-token :true) (make-instance 'literal-expr :value t))
   ((match-token :nil) (make-instance 'literal-expr :value nil)) ; Represented as Lisp nil

   ((match-token :number :string)
     (make-instance 'literal-expr :value (token-literal (previous-token))))

   ((match-token :identifier)
     (make-instance 'variable-expr :name (previous-token)))

   ((match-token :left-paren)
     (let ((expr (expression)))
       (consume :right-paren "Expect ')' after expression.")
       (make-instance 'grouping-expr :expression expr)))

   (t (error 'lox-parse-error :token (peek-token) :message "Expect expression."))))
