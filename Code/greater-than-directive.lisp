;;; 22.3.6.3 ~> End of justification or of logical block directive.

(cl:in-package #:sicl-format)

(define-directive #\> greater-than-directive (named-parameters-directive) ())

(define-format-directive-interpreter greater-than-directive
    ;; do nothing
    nil)
