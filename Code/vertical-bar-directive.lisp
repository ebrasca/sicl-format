;;; 22.3.1.4 ~| Page separators directive.

(cl:in-package #:sicl-format)

(define-directive #\| vertical-bar-directive (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter vertical-bar-directive
  (loop repeat how-many
        do (write-char #\Page *destination*)))

(define-format-directive-compiler vertical-bar-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (cond ((null how-many)
           `(loop repeat how-many
                  do (write-char #\Page *destination*)))
          ((< how-many 3)
           `(progn ,@(loop repeat how-many
                           collect `(write-char #\Page *destination*))))
          (t `(loop repeat ,how-many
                    do (write-char #\Page *destination*))))))
