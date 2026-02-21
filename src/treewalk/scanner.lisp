(in-package :cl-lox-treewalk)

(defparameter *keywords*
              (let ((table (make-hash-table :test 'equal)))
                (loop for (kw . type) in '(("and" . :AND) ("class" . :CLASS) ("else" . :ELSE) ("false" . :FALSE)
                                                          ("for" . :FOR) ("fun" . :FUN) ("if" . :IF) ("nil" . :NIL)
                                                          ("or" . :OR) ("print" . :PRINT) ("return" . :RETURN)
                                                          ("super" . :SUPER) ("this" . :THIS) ("true" . :TRUE)
                                                          ("var" . :VAR) ("while" . :WHILE)
                                                          ("break" . :BREAK) ("continue" . :CONTINUE))
                      do (setf (gethash kw table) type))
                table))

(defun scan-tokens (source)
  "Scans a string of Lox source code and returns a list of tokens."
  (let ((tokens nil)
        (start 0)
        (current 0)
        (line 1)
        (source-length (length source)))

    (labels ((is-at-end () (>= current source-length))
             (advance ()
                      (let ((c (char source current)))
                        (incf current)
                        c))
             (add-token (type &optional literal)
                        (let ((text (subseq source start current)))
                          (push (make-token :type type :lexeme text :literal literal :line line) tokens)))
             (match (expected)
                    (if (is-at-end) nil
                        (if (char= (char source current) expected)
                            (progn (incf current) t)
                            nil)))
             (peek ()
                   (if (is-at-end) #\Nul (char source current)))
             (peek-next ()
                        (if (>= (+ current 1) source-length) #\Nul (char source (+ current 1))))

             (string-builder ()
                             (loop while (and (not (char= (peek) #\")) (not (is-at-end)))
                                   do (if (char= (peek) #\Newline) (incf line))
                                     (advance))
                             (if (is-at-end)
                                 (error "Unterminated string at line ~A" line)
                                 (progn
                                  (advance) ; The closing "
                                  (let ((value (subseq source (+ start 1) (- current 1))))
                                    (add-token :string value)))))

             (is-digit (c)
                       (and (characterp c) (char<= #\0 c #\9)))
             (is-alpha (c)
                       (and (characterp c)
                            (or (char<= #\a c #\z)
                                (char<= #\A c #\Z)
                                (char= c #\_))))
             (is-alpha-numeric (c)
                               (or (is-alpha c) (is-digit c)))

             (number-builder ()
                             (loop while (is-digit (peek)) do (advance))
                             ;; Look for a fractional part
                             (when (and (char= (peek) #\.) (is-digit (peek-next)))
                                   (advance) ; Consume the "."
                                   (loop while (is-digit (peek)) do (advance)))
                             (add-token :number (read-from-string (subseq source start current))))

             (identifier-builder ()
                                 (loop while (is-alpha-numeric (peek)) do (advance))
                                 (let* ((text (subseq source start current))
                                        (kw-type (gethash (string-downcase text) *keywords*)))
                                   (add-token (or kw-type :identifier))))

             (scan-token ()
                         (let ((c (advance)))
                           (case c
                             (#\( (add-token :left-paren))
                             (#\) (add-token :right-paren))
                             (#\{ (add-token :left-brace))
                             (#\} (add-token :right-brace))
                             (#\, (add-token :comma))
                             (#\. (add-token :dot))
                             (#\- (add-token :minus))
                             (#\+ (add-token :plus))
                             (#\; (add-token :semicolon))
                             (#\* (add-token :star))
                             (#\! (add-token (if (match #\=) :bang-equal :bang)))
                             (#\= (add-token (if (match #\=) :equal-equal :equal)))
                             (#\< (add-token (if (match #\=) :less-equal :less)))
                             (#\> (add-token (if (match #\=) :greater-equal :greater)))
                             (#\/ (if (match #\/)
                                      ;; A comment goes until the end of the line
                                      (loop while (and (not (char= (peek) #\Newline)) (not (is-at-end)))
                                            do (advance))
                                      (add-token :slash)))
                             ((#\Space #\Return #\Tab) nil) ; Ignore whitespace
                             (#\Newline (incf line))
                             (#\" (string-builder))
                             (otherwise
                              (cond
                               ((is-digit c) (number-builder))
                               ((is-alpha c) (identifier-builder))
                               (t (error "Unexpected character ~A at line ~A" c line))))))))

      (loop while (not (is-at-end)) do
              (setf start current)
              (scan-token))

      (push (make-token :type :eof :lexeme "" :literal nil :line line) tokens)
      (nreverse tokens))))
