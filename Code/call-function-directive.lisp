;;; 22.3.5.4 ~/ Call function directive.

(cl:in-package #:sicl-format)

;;; This directive is particular in two different ways.  First, as
;;; with the "ignored newline" directive, there are characters
;;; belonging to the directive beyond the directive character itself,
;;; which means the standard mechanism of parsing it cannot be used.
;;; Second, this directive takes an arbitrary number of parameters.
;;;
;;; So, define-format-directive-interpreter cannot be used, since its
;;; main purpose is to give lexical access to each parameter by name.

(define-directive #\/ call-function-directive (directive)
    ()
  (%function-name :accessor function-name))

(defmethod check-directive-syntax progn ((directive call-function-directive))
  ;; Check that there is at most one package marker in the function name.
  ;; Also, compute a symbol from the name.
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp))
    directive
    ;; To figure out where the name of the function starts and ends,
    ;; we cannot search from the beginning of the directive, because
    ;; the given parameters can contain arbitrary characters following
    ;; a single quote (indicating a character parameter).  However,
    ;; we know that the last character of the directive is the trailing
    ;; #\/ of the function name, and the one preceding that is the
    ;; #\/ preceding the function name.
    (let ((pos1 (1+ (position #\/ control-string :end (1- end) :from-end t)))
          (pos2 (1- end)))
      (let ((position-of-first-package-marker
             (position #\: control-string :start pos1 :end pos2))
            (position-of-last-package-marker
             (position #\: control-string :start pos1 :end pos2 :from-end t)))
        (when (and (not (null position-of-first-package-marker))
                   (> position-of-last-package-marker
                      (1+ position-of-first-package-marker)))
          (error 'too-many-package-markers
                 :directive directive))
        ;; The HyperSpec says that all the characters of the function
        ;; name are treated as if they were upper-case.  It would
        ;; probably be smarter to follow the readtable-case of the
        ;; current readtable, but that's not what the spec says.
        (let ((package-name
               (if (null position-of-first-package-marker)
                   "COMMON-LISP-USER"
                   (string-upcase
                    (subseq control-string
                            pos1
                            position-of-first-package-marker))))
              (symbol-name
               (string-upcase
                (subseq control-string
                        (if (null position-of-first-package-marker)
                            pos1
                            (1+ position-of-last-package-marker))
                        pos2))))
          (let ((package (find-package package-name)))
            (when (null package)
              (error 'no-such-package
                     :directive directive))
            (multiple-value-bind (symbol status)
                (find-symbol symbol-name package)
              (when (or (null status)
                        (eq status :inherited))
                (error 'no-such-symbol
                       :directive directive))
              (when (and (= position-of-first-package-marker
                            position-of-last-package-marker)
                         (eq status :internal))
                (error 'symbol-not-external
                       :directive directive))
              (setf (function-name directive)
                    symbol))))))))

(defmethod interpret-format-directive ((directive call-function-directive))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp)
                   (at-signp at-signp)
                   (given-parameters given-parameters)
                   (function-name function-name))
    directive
    (let ((param-args
           (loop for parameter in given-parameters
                 collect (cond ((eq parameter '|#|)
                                (- (length *arguments*)
                                   *next-argument-pointer*))
                               ((eq parameter 'V)
                                (consume-next-argument t))
                               (t parameter)))))
      (apply function-name
             *destination*
             (consume-next-argument t)
             colonp
             at-signp
             param-args))))

;;; This is not quite right.  We should probably look up the
;;; function name at runtime as opposed to compile time.
(defmethod compile-format-directive ((directive call-function-directive))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp)
                   (at-signp at-signp)
                   (given-parameters given-parameters)
                   (function-name function-name))
    directive
    (let ((param-args
           (loop for parameter in given-parameters
                 collect (cond ((eq parameter '|#|)
                                `(- (length *arguments*)
                                    *next-argument-pointer*))
                               ((eq parameter 'V)
                                `(consume-next-argument t))
                               (t parameter)))))
      `(,function-name
        *destination*
        (consume-next-argument t)
        ,colonp
        ,at-signp
        ,@param-args))))
