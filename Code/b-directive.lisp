;;; 22.3.2.3 ~b Binary directive.

(cl:in-package #:sicl-format)

(define-directive #\b b-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

(define-format-directive-interpreter b-directive
  (print-radix-arg 2 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler b-directive
    `(print-radix-arg 2 ,colonp ,at-signp mincol padchar commachar comma-interval))
