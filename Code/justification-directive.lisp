;;; 22.3.6.2 ~< Justification directive.

(cl:in-package #:sicl-format)

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\< justification-directive (named-parameters-directive structured-directive-mixin)
    ((mincol :type (integer 0) :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)))
