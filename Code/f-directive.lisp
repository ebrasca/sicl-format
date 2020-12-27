;;; 22.3.3.1 ~f Fixed-format floating point directive.

(cl:in-package #:sicl-format)

(define-directive #\f f-directive (named-parameters-directive)
  ((w  :type integer)
   (d :type integer)
   (k :type (integer 0) :default-value 0)
   (overflowchar :type character)
   (padchar :type character :default-value #\Space)))

; (defun format-fixed (stream number w d k ovf pad atsign)

(defun print-float-arg (colonp at-signp w d k overflowchar padchar)
  (let ((argument (consume-next-argument t)))
    (if (numberp argument)
        (format *destination* "decimal ~D" argument)
        (if (floatp argument)
            (format *destination* "float!")
            (format *destination* "aesthetic ~A" argument)))))

      #|
      (if (floatp argument)
          (format-fixed-aux stream number w d k ovf pad atsign)
          (if (rationalp number)
              (format-fixed-aux stream
                                (coerce number 'single-float)
                                w d k ovf pad atsign)
              (format-write-field stream
                                  (decimal-string number)
                                  w 1 0 #\space t)))
      |#

(define-format-directive-interpreter f-directive
  (print-float-arg colonp at-signp w d k overflowchar padchar))

(define-format-directive-compiler f-directive
  `(print-float-arg ,colonp ,at-signp w d k overflowchar padchar))
