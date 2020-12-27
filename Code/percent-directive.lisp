;;; 22.3.1.2 ~% Newline directive.

(cl:in-package #:sicl-format)

(define-directive #\% percent-directive (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter percent-directive
  (loop repeat how-many
        do (terpri *destination*)))

(define-format-directive-compiler percent-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (cond ((null how-many)
           `(loop repeat how-many
                  do (terpri *destination*)))
          ((< how-many 3)
           `(progn ,@(loop repeat how-many
                           collect `(terpri *destination*))))
          (t `(loop repeat ,how-many
                    do (terpri *destination*))))))
