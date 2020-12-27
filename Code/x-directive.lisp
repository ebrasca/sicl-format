;;; 22.3.2.5 ~x Hexadecimal directive.

(cl:in-package #:sicl-format)

(define-directive #\x x-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

(define-format-directive-interpreter x-directive
  (print-radix-arg 16 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler x-directive
    `(print-radix-arg 16 ,colonp ,at-signp mincol padchar commachar comma-interval))
