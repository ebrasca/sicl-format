;;; 22.3.2.4 ~o Octal directive.

(cl:in-package #:sicl-format)

(define-directive #\o o-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

(define-format-directive-interpreter o-directive
  (print-radix-arg 8 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler o-directive
    `(print-radix-arg 8 ,colonp ,at-signp mincol padchar commachar comma-interval))
