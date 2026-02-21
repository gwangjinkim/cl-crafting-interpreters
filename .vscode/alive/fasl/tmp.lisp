(in-package :cl-lox-treewalk)

(defstruct token
  type
  lexeme
  literal
  line)

(defun make-new-token (type lexeme literal line)
  (make-token :type type :lexeme lexeme :literal literal :line line))
