;;; 22.3.1.1 ~c Character directive.

(cl:in-package #:sicl-format)

(define-directive #\c c-directive (named-parameters-directive) ())

(define-format-directive-interpreter c-directive
  (let ((char (consume-next-argument 'character)))
    (cond ((and (not colonp) (not at-signp))
           ;; Neither colon nor at-sign.
           ;; The HyperSpec says to do what write-char does.
           (write-char char *destination*))
          ((not at-signp)
           ;; We have only a colon modifier.
           ;; The HyperSpec says to do what write-char does for
           ;; printing characters, and what char-name does otherwise.
           ;; The definition of "printing char" is a graphic character
           ;; other than space.
           (if (and (graphic-char-p char) (not (eql char #\Space)))
               (write-char char *destination*)
               (princ (char-name char) *destination*)))
          ((not colonp)
           ;; We have only an at-sign modifier.
           ;; The HyperSpec says to print it the way the Lisp
           ;; reader can understand, which I take to mean "use PRIN1".
           ;; It also says to bind *print-escape* to t.
           (let ((*print-escape* t))
             (prin1 char *destination*)))
          (t
           ;; We have both a colon and and at-sign.
           ;; The HyperSpec says to do what ~:C does, but
           ;; also to mention unusual shift keys on the
           ;; keyboard required to type the character.
           ;; I don't see how to do that, so we do the same
           ;; as for ~:C.
           (if (and (graphic-char-p char) (not (eql char #\Space)))
               (write-char char *destination*)
               (princ (char-name char) *destination*))))))

(define-format-directive-compiler c-directive
  `(let ((char (consume-next-argument 'character)))
     ,(cond ((and (not colonp) (not at-signp))
             `(write-char char *destination*))
            ((not at-signp)
             `(if (and (graphic-char-p char) (not (eql char #\Space)))
                 (write-char char *destination*)
                 (princ (char-name char) *destination*)))
            ((not colonp)
             `(let ((*print-escape* t))
                (prin1 char *destination*)))
            (t
             `(if (and (graphic-char-p char) (not (eql char #\Space)))
                  (write-char char *destination*)
                  (princ (char-name char) *destination*))))))
