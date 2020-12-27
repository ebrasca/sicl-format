;;; 22.3.2.2 ~d Decimal directive.

(cl:in-package #:sicl-format)

(define-directive #\d d-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

(define-format-directive-interpreter d-directive
  (print-radix-arg 10 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler d-directive
    `(print-radix-arg 10 ,colonp ,at-signp mincol padchar commachar comma-interval))
