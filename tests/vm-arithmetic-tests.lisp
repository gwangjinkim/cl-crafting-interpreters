(in-package #:cl-lox-vm-tests)

(test vm-stack-arithmetic
      (cl-lox-vm:init-vm)
      (let ((chunk (cl-lox-vm:make-chunk)))
        ;; 1.2
        (let ((constant (cl-lox-vm:add-constant chunk 1.2d0)))
          (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-constant+ 123)
          (cl-lox-vm:write-chunk chunk constant 123))

        ;; 3.4
        (let ((constant (cl-lox-vm:add-constant chunk 3.4d0)))
          (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-constant+ 123)
          (cl-lox-vm:write-chunk chunk constant 123))

        ;; Add
        (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-add+ 123)

        ;; 5.6
        (let ((constant (cl-lox-vm:add-constant chunk 5.6d0)))
          (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-constant+ 123)
          (cl-lox-vm:write-chunk chunk constant 123))

        ;; Divide
        (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-divide+ 123)

        ;; Negate
        (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-negate+ 123)

        ;; Return
        (cl-lox-vm:write-chunk chunk cl-lox-vm:+op-return+ 123)

        (let ((result (cl-lox-vm:interpret chunk)))
          (is (eq :interpret-ok result))
          ;; Since run leaves the calculated value on the stack, we can verify it
          ;; The top of the stack should now be -( (1.2 + 3.4) / 5.6 ) => -0.8214285714285714d0
          (let ((final-value (cl-lox-vm::pop-stack)))
            (is (< (abs (- final-value -0.8214285714285714d0)) 1e-10))))

        (cl-lox-vm::free-chunk chunk))
      (cl-lox-vm:free-vm))
