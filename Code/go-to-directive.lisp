;;; 22.3.7.1 ~* Go to directive.

(cl:in-package #:sicl-format)

(define-directive #\* go-to-directive (named-parameters-directive at-most-one-modifier-mixin)
    ((param :type (integer 0))))

(define-format-directive-interpreter go-to-directive
  (cond (colonp
         ;; Back up in the list of arguments.
         ;; The default value for the parameter is 1.
         (let ((new-arg-pointer (- *next-argument-pointer* (or param 1))))
           (unless (>= new-arg-pointer 0)
             (error 'go-to-out-of-bounds
                    :what-argument new-arg-pointer
                    :max-arguments (length *arguments*)))
           (setf *next-argument-pointer* new-arg-pointer)))
        (at-signp
         ;; Go to an absolute argument number.
         ;; The default value for the parameter is 0.
         (let ((new-arg-pointer (or param 0)))
           (unless (<= 0 new-arg-pointer (length *arguments*))
             (error 'go-to-out-of-bounds
                    :what-argument new-arg-pointer
                    :max-arguments (length *arguments*)))
           (setf *next-argument-pointer* new-arg-pointer)))
        (t
         ;; Skip the next arguments.
         ;; The default value for the parameter is 1.
         (let ((new-arg-pointer (+ *next-argument-pointer* (or param 1))))
           (unless (<= new-arg-pointer (length *arguments*))
             (error 'go-to-out-of-bounds
                    :what-argument new-arg-pointer
                    :max-arguments (length *arguments*)))
           (setf *next-argument-pointer* new-arg-pointer)))))

(define-format-directive-compiler go-to-directive
  (cond (colonp
         ;; Back up in the list of arguments.
         ;; The default value for the parameter is 1.
         `(let ((new-arg-pointer (- *next-argument-pointer* (or param 1))))
            (unless (>= new-arg-pointer 0)
              (error 'go-to-out-of-bounds
                     :what-argument new-arg-pointer
                     :max-arguments (length *arguments*)))
            (setf *next-argument-pointer* new-arg-pointer)))
        (at-signp
         ;; Go to an absolute argument number.
         ;; The default value for the parameter is 0.
         `(let ((new-arg-pointer (or param 0)))
            (unless (<= 0 new-arg-pointer (length *arguments*))
              (error 'go-to-out-of-bounds
                     :what-argument new-arg-pointer
                     :max-arguments (length *arguments*)))
            (setf *next-argument-pointer* new-arg-pointer)))
        (t
         ;; Skip the next arguments.
         ;; The default value for the parameter is 1.
         `(let ((new-arg-pointer (+ *next-argument-pointer* (or param 1))))
            (unless (<= new-arg-pointer (length *arguments*))
              (error 'go-to-out-of-bounds
                     :what-argument new-arg-pointer
                     :max-arguments (length *arguments*)))
            (setf *next-argument-pointer* new-arg-pointer)))))
