;;; 22.3.7.6 ~? Recursive processing directive.

(cl:in-package #:sicl-format)

(define-directive #\? recursive-processing-directive (named-parameters-directive only-at-sign-mixin) ())

(define-format-directive-interpreter recursive-processing-directive
  (if at-signp
      ;; reuse the arguments from the parent control-string
      (format-with-runtime-arguments *destination*
                                     (consume-next-argument 'string))
      ;;
      (apply #'format
             *destination*
             (consume-next-argument 'string)
             (consume-next-argument 'list))))

(define-format-directive-compiler recursive-processing-directive
  (if at-signp
      ;; reuse the arguments from the parent control-string
      `(format-with-runtime-arguments *destination*
                                      (consume-next-argument 'string))
      ;;
      `(apply #'format
              *destination*
              (consume-next-argument 'string)
              (consume-next-argument 'list))))
