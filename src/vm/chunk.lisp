(in-package #:cl-lox-vm)

(deftype opcode () '(unsigned-byte 8))

;; Define Opcodes manually for now to act like C enums
(defconstant +op-return+ 0)
(defconstant +op-constant+ 1)
(defconstant +op-add+ 2)
(defconstant +op-subtract+ 3)
(defconstant +op-multiply+ 4)
(defconstant +op-divide+ 5)
(defconstant +op-negate+ 6)

(defstruct chunk
  "A dynamic array of bytecode instructions and constants."
  (capacity 0 :type fixnum)
  (count 0 :type fixnum)
  ;; The actual bytecode array is an unboxed array of unsigned 8-bit bytes
  (code (make-array 0 :element-type '(unsigned-byte 8) :adjustable t) :type (vector (unsigned-byte 8)))
  ;; Parallel array to track original source line numbers
  (lines (make-array 0 :element-type 'fixnum :adjustable t) :type (vector fixnum))
  ;; Constants pool
  (constants (make-value-array) :type value-array))

(defun write-chunk (chunk byte line)
  "Write an 8-bit byte to the chunk's code array."
  (when (= (chunk-capacity chunk) (chunk-count chunk))
        (let ((old-capacity (chunk-capacity chunk)))
          (let ((new-capacity (if (< old-capacity 8) 8 (* old-capacity 2))))
            (setf (chunk-capacity chunk) new-capacity)
            (adjust-array (chunk-code chunk) new-capacity)
            (adjust-array (chunk-lines chunk) new-capacity))))

  (setf (aref (chunk-code chunk) (chunk-count chunk)) byte)
  (setf (aref (chunk-lines chunk) (chunk-count chunk)) line)
  (incf (chunk-count chunk)))

(defun add-constant (chunk value)
  "Adds a value to a chunk's constant pool and returns its index."
  (write-value-array (chunk-constants chunk) value)
  (1- (value-array-count (chunk-constants chunk))))

(defun free-chunk (chunk)
  "Reinitializes the chunk, clearing its resources."
  (setf (chunk-code chunk) (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
  (setf (chunk-lines chunk) (make-array 0 :element-type 'fixnum :adjustable t))
  (setf (chunk-capacity chunk) 0)
  (setf (chunk-count chunk) 0)
  (free-value-array (chunk-constants chunk)))
