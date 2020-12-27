;;; 22.3.4.3 ~w Write directive.

(cl:in-package #:sicl-format)

(define-directive #\w w-directive (named-parameters-directive) ())

(define-format-directive-interpreter w-directive
  (cond ((and colonp at-signp )
         (let ((*print-pretty* t)
               (*print-level* nil)
               (*print-length* nil))
           (write (consume-next-argument t) :stream *destination*)))
        (colonp
         (let ((*print-pretty* t))
           (write (consume-next-argument t) :stream *destination*)))
        (at-signp
         (let ((*print-level* nil)
               (*print-length* nil))
           (write (consume-next-argument t) :stream *destination*)))
        (t
         (write (consume-next-argument t) :stream *destination*))))

(define-format-directive-compiler w-directive
  (cond ((and colonp at-signp )
         `(let ((*print-pretty* t)
                (*print-level* nil)
                (*print-length* nil))
            (write (consume-next-argument t) :stream *destination*)))
        (colonp
         `(let ((*print-pretty* t))
            (write (consume-next-argument t) :stream *destination*)))
        (at-signp
         `(let ((*print-level* nil)
                (*print-length* nil))
            (write (consume-next-argument t) :stream *destination*)))
        (t
         `(write (consume-next-argument t) :stream *destination*))))
