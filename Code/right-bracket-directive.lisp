;;; 22.3.7.3 ~] End of conditional expression directive.

(cl:in-package #:sicl-format)

(define-directive #\] right-bracket-directive (named-parameters-directive no-modifiers-mixin) ())

(define-format-directive-interpreter right-bracket-directive
    ;; do nothing
    nil)

(define-format-directive-compiler right-bracket-directive
    ;; do nothing
    nil)
