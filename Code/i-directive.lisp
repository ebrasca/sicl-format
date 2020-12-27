;;; 22.3.5.3 ~i Indent directive.

(cl:in-package #:sicl-format)

(define-directive #\i i-directive (named-parameters-directive)
    ((how-many :type (integer 0) :default-value 0)))

(define-format-directive-interpreter i-directive
  (pprint-indent (if colonp :current :block) how-many *destination*))

(define-format-directive-compiler i-directive
  `(pprint-indent ,(if colonp :current :block) how-many *destination*))
