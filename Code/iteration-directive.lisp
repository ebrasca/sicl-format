;;; 22.3.7.4 ~{ Iteration directive.

(cl:in-package #:sicl-format)

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\{ iteration-directive (named-parameters-directive structured-directive-mixin)
    ((iteration-limit :type (integer 0))))

(define-format-directive-interpreter iteration-directive
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let ((items (subseq (items directive) 0 (1- (length (items directive))))))
    (cond ((and colonp at-signp)
           ;; The remaining arguments should be lists.  Each argument
           ;; is used in a different iteration.
           (flet ((one-iteration ()
                    (let ((arg (aref *arguments* *next-argument-pointer*)))
                      (unless (listp arg)
                        (error 'argument-type-error
                               :expected-type 'list
                               :datum arg))
                      (let ((*arguments* (coerce arg 'vector))
                            (*next-argument-pointer* 0))
                        (interpret-items items))
                      (incf *next-argument-pointer*))))
             (if (null iteration-limit)
                 (loop until (= *next-argument-pointer* (length *arguments*))
                       do (one-iteration))
                 (loop until (= *next-argument-pointer* (length *arguments*))
                       repeat iteration-limit
                       do (one-iteration)))))
          (colonp
           ;; We use one argument, and that should be a list of sublists.
           ;; Each sublist is used as arguments for one iteration.
           (let ((arg (consume-next-argument 'list)))
             (flet ((one-iteration (args)
                      (unless (listp args)
                        (error 'argument-type-error
                               :expected-type 'list
                               :datum args))
                      (let ((*arguments* (coerce args 'vector))
                            (*next-argument-pointer* 0))
                        (interpret-items items))))
               (if (null iteration-limit)
                   (loop for args in arg ; a bit unusual naming perhaps
                         do (one-iteration args))
                   (loop for args in arg ; a bit unusual naming perhaps
                         repeat iteration-limit
                         do (one-iteration args))))))
          (at-signp
           (if (null iteration-limit)
               (loop until (= *next-argument-pointer* (length *arguments*))
                     do (interpret-items items))
               (loop until (= *next-argument-pointer* (length *arguments*))
                     repeat iteration-limit
                     do (interpret-items items))))
          (t
           ;; no modifiers
           ;; We use one argument, and that should be a list.
           ;; The elements of that list are used by the iteration.
           (let ((arg (consume-next-argument 'list)))
             (let ((*arguments* (coerce arg 'vector))
                   (*next-argument-pointer* 0))
               (if (null iteration-limit)
                   (loop until (= *next-argument-pointer* (length *arguments*))
                         do (interpret-items items))
                   (loop until (= *next-argument-pointer* (length *arguments*))
                         repeat iteration-limit
                         do (interpret-items items)))))))))

(define-format-directive-compiler iteration-directive
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let ((items (subseq (items directive) 0 (1- (length (items directive))))))
    (cond ((and colonp at-signp)
           ;; The remaining arguments should be lists.  Each argument
           ;; is used in a different iteration.
           `(flet ((one-iteration ()
                     (let ((arg (aref *arguments* *next-argument-pointer*)))
                       (unless (listp arg)
                         (error 'argument-type-error
                                :expected-type 'list
                                :datum arg))
                       (let ((*arguments* (coerce arg 'vector))
                             (*next-argument-pointer* 0))
                         ,@(compile-items items))
                       (incf *next-argument-pointer*))))
              ,(if (null iteration-limit)
                   `(loop until (= *next-argument-pointer* (length *arguments*))
                          do (one-iteration))
                   `(loop until (= *next-argument-pointer* (length *arguments*))
                          repeat ,iteration-limit
                          do (one-iteration)))))
          (colonp
           ;; We use one argument, and that should be a list of sublists.
           ;; Each sublist is used as arguments for one iteration.
           `(let ((arg (consume-next-argument 'list)))
              (flet ((one-iteration (args)
                       (unless (listp args)
                         (error 'argument-type-error
                                :expected-type 'list
                                :datum args))
                       (let ((*arguments* (coerce args 'vector))
                             (*next-argument-pointer* 0))
                         ,@(compile-items items))))
                ,(if (null iteration-limit)
                     `(loop for args in arg ; a bit unusual naming perhaps
                            do (one-iteration args))
                     `(loop for args in arg ; a bit unusual naming perhaps
                            repeat ,iteration-limit
                            do (one-iteration args))))))
          (at-signp
           (if (null iteration-limit)
               `(loop until (= *next-argument-pointer* (length *arguments*))
                      do (progn ,@(compile-items items)))
               `(loop until (= *next-argument-pointer* (length *arguments*))
                      repeat ,iteration-limit
                      do (progn ,@(compile-items items)))))
          (t
           ;; no modifiers
           ;; We use one argument, and that should be a list.
           ;; The elements of that list are used by the iteration.
           `(let ((arg (consume-next-argument 'list)))
              (let ((*arguments* (coerce arg 'vector))
                    (*next-argument-pointer* 0))
                ,(if (null iteration-limit)
                     `(loop until (= *next-argument-pointer* (length *arguments*))
                            do (progn ,@(compile-items items)))
                     `(loop until (= *next-argument-pointer* (length *arguments*))
                            repeat iteration-limit
                            do (progn ,@(compile-items items))))))))))
