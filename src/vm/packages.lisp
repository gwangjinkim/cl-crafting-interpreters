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
          #:op-return
          #:op-constant))

(in-package #:cl-lox-vm)
