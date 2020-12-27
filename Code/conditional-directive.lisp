;;; 22.3.7.2 ~[ Conditional expression directive.

(cl:in-package #:sicl-format)

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\[ conditional-directive
    (named-parameters-directive structured-directive-mixin at-most-one-modifier-mixin)
    ((param :type integer))
  (%clauses :accessor clauses)
  (%last-clause-is-default-p :initform nil :accessor last-clause-is-default-p))

(defmethod check-directive-syntax progn ((directive conditional-directive))
  ;; Check that, if a parameter is given, then there are
  ;; no modifiers.
  (when (and (not (null (given-parameters directive)))
             (or (colonp directive) (at-signp directive)))
    (error 'modifier-and-parameter
           :directive directive))
  ;; eliminate the end-of-conditional directive from the items
  (let ((items (subseq (items directive) 0 (1- (length (items directive))))))
    ;; Check that there is at least one item in items
    (when (zerop (length items))
      (error 'at-least-one-item-required
             :directive directive))
    ;; Check that, if a colon modifier was given, then
    ;; there should be a single clause separator (two clauses).
    (when (and (colonp directive)
               (/= (count-if (lambda (item) (typep item 'semicolon-directive))
                             items)
                   1))
      (error 'colon-modifier-requires-two-clauses))
    ;; Check that, if an at-sign modifier was given, then
    ;; there should be a no clause separators (a single clause).
    (when (and (at-signp directive)
               (/= (count-if (lambda (item) (typep item 'semicolon-directive))
                             items)
                   0))
      (error 'at-sign-modifier-requires-one-clause))
    (flet ((clause-separator-with-colon-modifier (item)
             (and (typep item 'semicolon-directive)
                  (colonp item))))
      (let ((position-of-clause-with-colon-modifier
             (position-if #'clause-separator-with-colon-modifier
                          items
                          :from-end t)))
        ;; Check that, if a modifier is given, then there should
        ;; be no clause separator with colon modifier.
        (when (and (or (colonp directive) (at-signp directive))
                   (not (null position-of-clause-with-colon-modifier)))
          (error 'clause-separator-with-colon-modifier-not-allowed
                 :directive directive))
        (when (or
               ;; Check whether there is more than one clause separator
               ;; with a `:' modifier.
               (> (count-if #'clause-separator-with-colon-modifier items)
                  1)
               ;; Check whether the clause separator with a `:' modifier
               ;; (if any) is not the last one.
               (and (find-if #'clause-separator-with-colon-modifier items)
                    (/= position-of-clause-with-colon-modifier
                        (position-if (lambda (item) (typep item 'semicolon-directive))
                                     items
                                     :from-end t))))
          (error 'illegal-clause-separators
                 :directive directive))
        (unless (null position-of-clause-with-colon-modifier)
          (setf (last-clause-is-default-p directive) t))
        ;; Divide the items into clauses.
        ;; Each clause is just a vector of items.
        (loop with start = 0
              with end = (length items)
              with clauses = '()
              until (= start end)
              do (let ((position-of-clause-separator
                        (position-if (lambda (item) (typep item 'semicolon-directive))
                                     items
                                     :start start)))
                   (if (null position-of-clause-separator)
                       (progn (push (subseq items start) clauses)
                              (setf start end))
                       (progn (push (subseq items
                                            start
                                            position-of-clause-separator)
                                    clauses)
                              (setf start (1+ position-of-clause-separator)))))
              finally (setf (clauses directive)
                            (coerce (nreverse clauses) 'vector)))))))

(define-format-directive-interpreter conditional-directive
  (cond (at-signp
         (when (>= *next-argument-pointer* (length *arguments*))
           (error 'no-more-arguments))
         (if (aref *arguments* *next-argument-pointer*)
             ;; Then do not consume the argument and
             ;; process the clause.
             (interpret-items (aref (clauses directive) 0))
             ;; Else, consume the argument and
             ;; do not process the clause
             (incf *next-argument-pointer*)))
        (colonp
         (interpret-items (aref (clauses directive)
                                (if (consume-next-argument t)
                                    ;; Then interpret the first clause
                                    ;; (yes that's what the CLHS says)
                                    0
                                    ;; Else interpret the second clause
                                    1))))
        (t
         ;; If a parameter was given, use it,
         ;; else use the next argument.
         (let ((val (or param (consume-next-argument 'integer))))
           (if (or (minusp val)
                   (>= val (length (clauses directive))))
               ;; Then the argument is out of range
               (when (last-clause-is-default-p directive)
                 ;; Then execute the default-clause
                 (interpret-items (aref (clauses directive)
                                        (1- (length (clauses directive))))))
               ;; Else, execute the corresponding clause
               (interpret-items (aref (clauses directive) val)))))))

(define-format-directive-compiler conditional-directive
  (cond (at-signp
         `(progn (when (>= *next-argument-pointer* (length *arguments*))
                   (error 'no-more-arguments))
                 (if (aref *arguments* *next-argument-pointer*)
                     ;; Then do not consume the argument and
                     ;; process the clause.
                     (progn ,@(compile-items (aref (clauses directive) 0))
                     ;; Else, consume the argument and
                     ;; do not process the clause
                     (incf *next-argument-pointer*)))))
        (colonp
         `(if (consume-next-argument t)
              ;; Compile the first clause
              ;; (yes that's what the CLHS says)
              (progn ,@(compile-items (aref (clauses directive) 0)))
              ;; Compile the second clause
              (progn ,@(compile-items (aref (clauses directive) 1)))))
        (t
         ;; If a parameter was given, use it,
         ;; else use the next argument.
         `(let ((val (or param (consume-next-argument 'integer))))
            (if (or (minusp val)
                    (>= val ,(length (clauses directive))))
                ;; Then the argument is out of range
                ,(when (last-clause-is-default-p directive)
                       ;; Then execute the default-clause
                       `(progn ,@(compile-items
                                  (aref (clauses directive)
                                        (1- (length (clauses directive)))))))
                ;; Else, execute the corresponding clause
                (case val
                  ,@(loop for i from 0 below (length (clauses directive))
                          for clause across (clauses directive)
                          collect `(,i ,@(compile-items
                                          (aref (clauses directive) i))))))))))
