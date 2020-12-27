;;; 22.3.7.5 ~} End of iteration directive.

(cl:in-package #:sicl-format)

(define-directive #\} right-brace-directive (named-parameters-directive only-colon-mixin) ())

(define-format-directive-interpreter right-brace-directive
    ;; do nothing
    nil)

(define-format-directive-compiler right-brace-directive
    ;; do nothing
    nil)
