(in-package :cl-lox-treewalk)

(defclass environment ()
    ((enclosing :initarg :enclosing
                :accessor environment-enclosing
                :initform nil
                :documentation "The parent environment, if any.")
     (values :initform (make-hash-table :test 'equal)
       :accessor environment-values
       :documentation "A hash table storing variable names and their values."))
  (:documentation "Represents a Lox lexical scope."))

(defun make-environment (&optional enclosing)
  "Creates a new environment, optionally enclosed by another API environment."
  (make-instance 'environment :enclosing enclosing))

(defmethod define-variable ((env environment) name value)
  "Binds a new variable or reassigns an existing one in the CURRENT scope."
  ;; name should be a string (the lexeme)
  (setf (gethash name (environment-values env)) value))

(defmethod get-variable ((env environment) name token)
  "Looks up a variable's value, traversing up the environment chain."
  (multiple-value-bind (value present) (gethash name (environment-values env))
    (if present
        value
        (if (environment-enclosing env)
            (get-variable (environment-enclosing env) name token)
            (runtime-error token (format nil "Undefined variable '~A'." name))))))

(defmethod assign-variable ((env environment) name value token)
  "Redefines an EXISTING variable. Traverses up the chain."
  (multiple-value-bind (old-value present) (gethash name (environment-values env))
    (declare (ignore old-value))
    (if present
        (setf (gethash name (environment-values env)) value)
        (if (environment-enclosing env)
            (assign-variable (environment-enclosing env) name value token)
            (runtime-error token (format nil "Undefined variable '~A'." name))))))
