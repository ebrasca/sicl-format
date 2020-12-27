;;; 22.3.1.5 ~~ Tildes directive.

(cl:in-package #:sicl-format)

(define-directive #\~ tilde-directive (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter tilde-directive
  (loop repeat how-many
        do (write-char #\~ *destination*)))

(define-format-directive-compiler tilde-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (cond ((null how-many)
           `(loop repeat how-many
                  do (write-char #\~ *destination*)))
          ((< how-many 3)
           `(progn ,@(loop repeat how-many
                           collect `(write-char #\~ *destination*))))
          (t `(loop repeat ,how-many
                    do (write-char #\~ *destination*))))))
