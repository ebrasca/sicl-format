;;; 22.3.5.2 ~< Logical block directive.

(cl:in-package #:sicl-format)

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\< logical-block-directive (named-parameters-directive structured-directive-mixin) ())
