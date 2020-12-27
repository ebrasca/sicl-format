;;; 22.3.9.3 ~Newline Igored newline directive.

(cl:in-package #:sicl-format)

(define-directive #\Newline newline-directive (named-parameters-directive at-most-one-modifier-mixin) ())

(define-format-directive-interpreter newline-directive
  (cond (colonp
         ;; remove the newline but print the following whitespace
         (let ((start (1+ (position #\Newline control-string :start start))))
           (loop for index from start below end
                 do (write-char (char control-string index) *destination*))))
        (at-signp
         ;; print the newline, but remove the following whitespace
         (write-char #\Newline *destination*))
        (t
         ;; ignore both the newline and the following whitespace
         nil)))

(define-format-directive-compiler newline-directive
  (cond (colonp
         ;; remove the newline but print the following whitespace
         `(let ((start (1+ (position #\Newline control-string :start start))))
            (write-string ,(subseq control-string start end) *destination*)))
        (at-signp
         ;; print the newline, but remove the following whitespace
         `(write-char #\Newline *destination*))
        (t
         ;; ignore both the newline and the following whitespace
         nil)))
