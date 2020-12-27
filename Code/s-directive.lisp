;;; 22.3.4.2 ~s Standard directive.

(cl:in-package #:sicl-format)

(define-directive #\s s-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)))

(define-format-directive-interpreter s-directive
  (let* ((*print-escape* t)
         (raw-output
          (let ((arg (consume-next-argument t)))
            (if (and colonp (null arg))
                "()"
                (prin1-to-string arg)))))
    (print-a-or-s raw-output at-signp mincol colinc minpad padchar)))

(define-format-directive-compiler s-directive
  `(let* ((*print-escape* t)
          (raw-output
           (let ((arg (consume-next-argument t)))
             ,(if colonp
                  `(if (null arg)
                       "()"
                       (princ-to-string arg))
                  `(prin1-to-string arg)))))
     (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
       ,(if at-signp
            `(progn (loop repeat pad-length do (write-char padchar *destination*))
                    (write-string raw-output *destination*))
            `(progn (write-string raw-output *destination*)
                    (loop repeat pad-length do (write-char padchar *destination*)))))))
