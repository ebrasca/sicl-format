;;; 22.3.5.1 ~_ Conditional newline directive.

(cl:in-package #:sicl-format)

(define-directive #\_ underscore-directive (named-parameters-directive) ())

(define-format-directive-interpreter underscore-directive
  (pprint-newline (cond ((and colonp at-signp) :mandatory)
                        (colonp :fill)
                        (at-signp :miser)
                        (t :linear))
                  *destination*))

(define-format-directive-compiler underscore-directive
  `(pprint-newline ,(cond ((and colonp at-signp) :mandatory)
                          (colonp :fill)
                          (at-signp :miser)
                          (t :linear))
                   *destination*))
