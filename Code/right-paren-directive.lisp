;;; 22.3.8.2 ~) End of case conversion directive.

(cl:in-package #:sicl-format)

(define-directive #\) right-paren-directive (named-parameters-directive no-modifiers-mixin) ())

(define-format-directive-interpreter right-paren-directive
    ;; do nothing
    nil)

(define-format-directive-compiler right-paren-directive
    ;; do nothing
    nil)
