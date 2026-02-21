(in-package :cl-lox-treewalk)

(defun run-file (path)
  "Run a Lox source file."
  (format t "Running file: ~A~%" path))

(defun run-prompt ()
  "Run the interactive Lox REPL."
  (loop
   (format t "> ")
   (force-output)
   (let ((line (read-line *standard-input* nil nil)))
     (unless line (return))
     (format t "Executed: ~A~%" line))))

(defun main ()
  "Entry point for the Tree-walk Lox interpreter."
  (let ((args sb-ext:*posix-argv*))
    (cond
     ((> (length args) 2)
       (format *error-output* "Usage: lox-tw [script]~%")
       (sb-ext:exit :code 64))
     ((= (length args) 2)
       (run-file (second args)))
     (t
       (run-prompt)))))
