;;; 22.3.8.1 ~( Case conversion directive.

(cl:in-package #:sicl-format)

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\( case-conversion-directive (named-parameters-directive structured-directive-mixin) ())

(define-format-directive-interpreter case-conversion-directive
  (let ((output (with-output-to-string (stream)
                  (let ((*destination* stream))
                    (interpret-items (items directive))))))
    (cond ((and colonp at-signp)
           (nstring-upcase output))
          (colonp
           (nstring-capitalize output))
          (at-signp
           (let ((pos (position-if #'alphanumericp output)))
             (when (not (null pos))
               (setf (char output pos)
                     (char-upcase (char output pos))))))
          (t
           (nstring-downcase output)))
    (princ output *destination*)))

(define-format-directive-compiler case-conversion-directive
  `(let ((output (with-output-to-string (stream)
                   (let ((*destination* stream))
                     ,@(compile-items (items directive))))))
     ,(cond ((and colonp at-signp)
             `(nstring-upcase output))
            (colonp
             `(nstring-capitalize output))
            (at-signp
             `(let ((pos (position-if #'alphanumericp output)))
                (when (not (null pos))
                  (setf (char output pos)
                        (char-upcase (char output pos))))))
            (t
             `(nstring-downcase output)))
     (princ output *destination*)))
