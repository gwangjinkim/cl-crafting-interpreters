(defpackage #:cl-lox-vm
  (:use #:cl)
  (:export ; Chunk management
          #:chunk
          #:make-chunk
          #:write-chunk
          #:add-constant

          ; Debugging/Disassembly
          #:disassemble-chunk
          #:disassemble-instruction

          ; VM state
          #:init-vm
          #:free-vm
          #:interpret

          ; OpCodes (enumerated below)
          #:+op-return+
          #:+op-constant+
          #:+op-add+
          #:+op-subtract+
          #:+op-multiply+
          #:+op-divide+
          #:+op-negate+))

(in-package #:cl-lox-vm)
