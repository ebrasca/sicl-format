;;; 22.3.8.3 ~p Plural directive.

(cl:in-package #:sicl-format)

(define-directive #\p plural-directive (named-parameters-directive) ())

(define-format-directive-interpreter plural-directive
  (when colonp
    (when (zerop *next-argument-pointer*)
      (error 'go-to-out-of-bounds
             :what-argument -1
             :max-arguments (length *arguments*)))
    (decf *next-argument-pointer*))
  (if at-signp
      (princ (if (eql (consume-next-argument t) 1)
                 "y"
                 "ies")
             *destination*)
      (when (eql (consume-next-argument t) 1)
        (write-char #\s *destination*))))

(define-format-directive-compiler plural-directive
  (when colonp
    `(progn (when (zerop *next-argument-pointer*)
              (error 'go-to-out-of-bounds
                     :what-argument -1
                     :max-arguments (length *arguments*)))
            (decf *next-argument-pointer*)))
  (if at-signp
      `(princ (if (eql (consume-next-argument t) 1)
                  "y"
                  "ies")
              *destination*)
      `(when (eql (consume-next-argument t) 1)
         (write-char #\s *destination*))))
