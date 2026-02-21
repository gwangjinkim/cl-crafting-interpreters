(in-package #:cl-lox-vm)

(defconstant +stack-max+ 256)

(defstruct vm
  "The main Virtual Machine state."
  (chunk nil :type (or null chunk))
  (ip 0 :type fixnum)
  (stack (make-array +stack-max+ :element-type 'value) :type (vector value))
  (stack-top 0 :type fixnum))

(defvar *vm* nil "The global VM instance.")

(defun init-vm ()
  "Initializes the global virtual machine."
  (setf *vm* (make-vm)))

(defun free-vm ()
  "Frees resources held by the global virtual machine."
  (setf *vm* nil))

(defun push-stack (value)
  "Pushes a value onto the VM stack."
  (setf (aref (vm-stack *vm*) (vm-stack-top *vm*)) value)
  (incf (vm-stack-top *vm*)))

(defun pop-stack ()
  "Pops and returns a value from the VM stack."
  (decf (vm-stack-top *vm*))
  (aref (vm-stack *vm*) (vm-stack-top *vm*)))

(defun run ()
  "The core bytecode execution loop."
  (let* ((chunk (vm-chunk *vm*))
         (code (chunk-code chunk))
         (constants (value-array-values (chunk-constants chunk))))
    (loop
     #+debug-trace-execution
     (progn
      (format t "          ")
      (loop for i from 0 below (vm-stack-top *vm*)
            do (format t "[ ~G ]" (aref (vm-stack *vm*) i)))
      (format t "~%")
      (disassemble-instruction chunk (vm-ip *vm*)))

     (let ((instruction (aref code (vm-ip *vm*))))
       (incf (vm-ip *vm*))

       (cond
        ((= instruction +op-return+)
          ;; For now, just exit the loop
          #-debug-trace-execution
          (return-from run :interpret-ok)
          #+debug-trace-execution
          (return-from run :interpret-ok))

        ((= instruction +op-constant+)
          (let ((constant-index (aref code (vm-ip *vm*))))
            (incf (vm-ip *vm*))
            (push-stack (aref constants constant-index))))

        ((= instruction +op-add+)
          (let ((b (pop-stack)) (a (pop-stack)))
            (push-stack (+ a b))))

        ((= instruction +op-subtract+)
          (let ((b (pop-stack)) (a (pop-stack)))
            (push-stack (- a b))))

        ((= instruction +op-multiply+)
          (let ((b (pop-stack)) (a (pop-stack)))
            (push-stack (* a b))))

        ((= instruction +op-divide+)
          (let ((b (pop-stack)) (a (pop-stack)))
            (push-stack (/ a b))))

        ((= instruction +op-negate+)
          (push-stack (- (pop-stack))))

        (t (error "Unknown opcode!")))))))

(defun interpret (chunk)
  "Interprets a chunk of bytecode."
  (setf (vm-chunk *vm*) chunk)
  (setf (vm-ip *vm*) 0)
  (run))
