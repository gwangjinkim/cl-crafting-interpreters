(in-package #:cl-lox-vm)

;; For now, Lox 'Value' is just a normal Common Lisp double-float
(deftype value () 'double-float)

(defun print-value (value &optional (stream t))
  "Prints a Lox value to the given stream."
  (format stream "~G" value))

(defstruct value-array
  "A dynamic array for holding Lox values (constants pool)."
  (capacity 0 :type fixnum)
  (count 0 :type fixnum)
  (values (make-array 0 :element-type 'value :adjustable t) :type (vector value)))

(defun write-value-array (array value)
  "Appends a value to the value-array, resizing if necessary."
  (when (= (value-array-capacity array) (value-array-count array))
        (let ((old-capacity (value-array-capacity array)))
          (let ((new-capacity (if (< old-capacity 8) 8 (* old-capacity 2))))
            (setf (value-array-capacity array) new-capacity)
            (adjust-array (value-array-values array) new-capacity))))
  (setf (aref (value-array-values array) (value-array-count array)) value)
  (incf (value-array-count array)))

(defun free-value-array (array)
  "Frees the resources of a value array."
  (setf (value-array-values array) (make-array 0 :element-type 'value :adjustable t))
  (setf (value-array-capacity array) 0)
  (setf (value-array-count array) 0))
