;;; 22.3.1.3 ~& Fresh line and newlines directive.

(cl:in-package #:sicl-format)

(define-directive #\& ampersand-directive (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter ampersand-directive
  (unless (zerop how-many)
    (fresh-line *destination*)
    (loop repeat (1- how-many)
          do (terpri *destination*))))

(define-format-directive-compiler ampersand-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (cond ((null how-many)
           `(unless (zerop how-many)
              (fresh-line *destination*)
              (loop repeat (1- how-many)
                    do (terpri *destination*))))
          ((zerop how-many)
           nil)
          ((< how-many 3)
           `(progn (fresh-line *destination*)
                   ,@(loop repeat (1- how-many)
                           collect `(terpri *destination*))))
          (t `(progn (fresh-line *destination*)
                     (loop repeat ,(1- how-many)
                           do (terpri *destination*)))))))
