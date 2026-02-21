(in-package #:cl-lox-vm)

(defun disassemble-chunk (chunk name stream)
  "Disassembles all instructions in a chunk in a readable format."
  (format stream "== ~A ==~%" name)
  (let ((offset 0))
    (loop while (< offset (chunk-count chunk))
          do (setf offset (disassemble-instruction chunk offset stream)))))

(defun disassemble-instruction (chunk offset &optional (stream t))
  "Disassembles a single instruction at the given offset."
  (format stream "~4,'0d " offset)

  ;; Print line number, or '|' if it's the same as the previous line
  (let ((line (aref (chunk-lines chunk) offset)))
    (if (and (> offset 0) (= line (aref (chunk-lines chunk) (1- offset))))
        (format stream "   | ")
        (format stream "~4d " line)))

  (let ((instruction (aref (chunk-code chunk) offset)))
    (cond
     ((= instruction +op-return+) (simple-instruction "OP_RETURN" offset stream))
     ((= instruction +op-constant+) (constant-instruction "OP_CONSTANT" chunk offset stream))
     ((= instruction +op-add+) (simple-instruction "OP_ADD" offset stream))
     ((= instruction +op-subtract+) (simple-instruction "OP_SUBTRACT" offset stream))
     ((= instruction +op-multiply+) (simple-instruction "OP_MULTIPLY" offset stream))
     ((= instruction +op-divide+) (simple-instruction "OP_DIVIDE" offset stream))
     ((= instruction +op-negate+) (simple-instruction "OP_NEGATE" offset stream))
     (t
       (format stream "Unknown opcode ~A~%" instruction)
       (1+ offset)))))

(defun simple-instruction (name offset stream)
  "Disassemble an instruction with no operands."
  (format stream "~A~%" name)
  (1+ offset))

(defun constant-instruction (name chunk offset stream)
  "Disassemble an instruction with one constant operand."
  (let* ((constant-index (aref (chunk-code chunk) (1+ offset)))
         (constant-value (aref (value-array-values (chunk-constants chunk)) constant-index)))
    (format stream "~16Aa ~4,'0d '" name constant-index)
    (print-value constant-value stream)
    (format stream "'~%")
    (+ offset 2)))
