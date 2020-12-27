;;;; Copyright (c) 2008.
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or
;;;; without modification, are permitted provided that the following
;;;; conditions are met:
;;;;
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above
;;;;    copyright notice, this list of conditions and the following
;;;;    disclaimer in the documentation and/or other materials
;;;;    provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.


;;; The name of this project is SICL, which doesn't stand for anything
;;; in particular.  Pronounce it like "sickle".
;;;
;;; The purpose of this code is to provide a totally portable
;;; implementation of some high-level functionality of the Common Lisp
;;; language, so that implementors of Common Lisp systems can
;;; integrate it as it is into their systems, without having to
;;; implement and maintain a specific version of it.
;;;
;;; A portable implementation of the Common Lisp FORMAT function.
;;;
;;; Status:
;;;
;;;   Not all directives are implemented.
;;;
;;; TODO:
;;;
;;;   * Implement the last couple of directives.
;;;
;;;   * Implement the directive compiler.
;;;
;;;   * Improve some of the condition reports.
;;;
;;;   * We might want to use reader conditionals to determine how
;;;     to handle things like counting colums (for the TAB directive),
;;;     because it could be costly (and/or imprecise) to count each
;;;     character output.
;;;
;;;
;;; To think about:
;;;
;;;   * Should we use ASSERT as opposed to ERROR to get correctable
;;;     errors?
;;;
;;;   * Should we put in restarts?
;;;
;;;   * What do we do about the possibility that the syntax categories
;;;     of some characters might be altered (for ignored newline
;;;     directive)?

(cl:in-package #:sicl-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing a control string

;;; Parse a parameter.  This function is called only if a parameter
;;; should be there, either because it is the first possible parameter
;;; position, and we have verified that the character at the start
;;; position is a member of "',vV#+-0123456789" or because the
;;; previous character was a comma, so there ought to be a parameter
;;; next.  If no parameter is found, signal an error.  Return two
;;; values, the parameter that was parsed and the position immediately
;;; beyond the parameter that was parsed.
(defun parse-parameter (string start end tilde-position)
  (cond ((= start end)
         (error 'end-of-control-string-error
                :control-string string
                :tilde-position tilde-position
                :why "expected a parameter"))
        ((eql (char string start) #\,)
         ;; Indicates absence of parameter.
         (values nil start))
        ((or (eql (char string start) #\v) (eql (char string start) #\V))
         ;; Indicates that the value is to be taken from the arguments.
         (values 'v (1+ start)))
        ((eql (char string start) #\#)
         ;; Indicates that the value is the remaining number of arguments
         (values '|#| (1+ start)))
        ((eql (char string start) #\')
         (incf start)
         (when (= start end)
           (error 'end-of-control-string-error
                  :control-string string
                  :tilde-position tilde-position
                  :why "character expected"))
         (values (char string start) (1+ start)))
        ((find (char string start) "+-0123456789")
         (multiple-value-bind (value position)
             (parse-integer string :start start :junk-allowed t)
           (when (null value)
             (error 'expected-integer-error
                    :control-string string
                    :tilde-position tilde-position
                    :index start))
           (values value position)))
        (t
         (error 'expected-parameter-start
                :control-string string
                :tilde-position tilde-position
                :index start))))

;;; Parse the parameters of a format directive.  STRING is the entire
;;; control string START is the position of the tilde character that
;;; starts the directive.  END is the length of the control string.
;;; Return the list of parameters and the position immediately beyond
;;; the last parameter.
(defun parse-parameters (string start end)
  (let ((position (1+ start))
        (parameters '()))
    (when (find (char string position) "',vV#+-0123456789")
      (multiple-value-bind (parameter pos)
          (parse-parameter string position end start)
        (push parameter parameters)
        (setf position pos))
      (loop while (and (not (= position end))
                       (eql (char string position) #\,))
            do (progn (incf position)
                      (multiple-value-bind (parameter pos)
                          (parse-parameter string position end start)
                        (push parameter parameters)
                        (setf position pos)))))
    (values (nreverse parameters) position)))

;;; Parse the modifiers of a format directive.  The colon and at-sign
;;; modifiers are optional and can appear in any order.  However, we
;;; do not allow more than one of each kind.  Return three values, a
;;; boolean indicating whether the colon modifier was found, a boolean
;;; indicating whether the at-sign modifier was found, and the first
;;; position beyond the modifiers in the string.
(defun parse-modifiers (string start end tilde-position)
  (let ((position (position-if-not (lambda (char)
                                     (or (eql char #\@)
                                         (eql char #\:)))
                                   string
                                   :start start)))
    (when (null position)
      (setf position end))
    (cond ((= position start)
           (values nil nil start))
          ((= position (1+ start))
           (if (eql (char string start) #\:)
               (values t nil (1+ start))
               (values nil t (1+ start))))
          ((= position (+ start 2))
           (if (eql (char string start)
                    (char string (1+ start)))
               (error 'two-identical-modifiers
                      :control-string string
                      :tilde-position tilde-position
                      :index start)
               (values t t (+ start 2))))
          (t
           (error 'more-than-two-modifiers
                  :control-string string
                  :tilde-position tilde-position
                  :index start)))))

;;; Parse a format directive.  The string is a format control string.
;;; The start position is the position of the tilde character that
;;; starts the directive.  Return the the character indicating the
;;; directive, a list of format parameters, two booleans indicating
;;; whether the colon and the at-sign modifiers were given, and the
;;; position in the string immediately beyond the character indicating
;;; the directive.
(defun parse-format-directive (string start)
  (let ((end (length string)))
    (multiple-value-bind (parameters position1)
        (parse-parameters string start end)
      (multiple-value-bind (colonp at-signp position2)
          (parse-modifiers string position1 end start)
        (when (= position2 end)
          (error 'end-of-control-string-error
                 :control-string string
                 :tilde-position start
                 :why "expected a letter corresponding to a format directive"))
        ;; We need to handle the special cases of the ~Newline and ~/
        ;; directives, because those directive comprise characters
        ;; that follow the directive character itself.
        (let ((directive-character (char string position2)))
          (incf position2)
          (cond ((eql directive-character #\Newline)
                 ;; I think we must assume standard syntax here, because
                 ;; there is no portable way of checking the syntax type of
                 ;; a character.
                 (loop while (and (< position2 end)
                                  (find (char string position2)
                                        #(#\Space #\Tab #\Page #\Return)))
                  do (incf position2)))
                ((eql directive-character #\/)
                 (let ((position-of-trailing-slash
                        (position #\/ string :start position2)))
                   (when (null position-of-trailing-slash)
                     (error 'end-of-control-string-error
                            :control-string string
                            :tilde-position start
                            :why "expected a trailing slash"))
                   (setf position2 (1+ position-of-trailing-slash)))))
          (values directive-character parameters colonp at-signp position2))))))

;;; How we represent a directive.  It may seem wasteful to allocate
;;; a class instance for each directive, but most format directives
;;; are handled at compile time anyway.
(defclass directive ()
  (;; the entire control string in which this directive was found
   (%control-string :initarg :control-string :reader control-string)
   ;; the position in the control string of the ~ character.
   (%start :initarg :start :reader start)
   ;; the first position beyond the directive character
   (%end :initarg :end :reader end)
   ;; The directive character used.
   (%directive-character :initarg :directive-character :reader directive-character)
   ;; a list of parameters, each one is either an integer or a character
   (%given-parameters :initarg :given-parameters :reader given-parameters)
   ;; true if and only if the `:' modifier was given
   (%colonp :initarg :colonp :reader colonp)
   ;; true if and only if the `@' modifier was given
   (%at-signp :initarg :at-signp :reader at-signp)))

;;; The base class of all directives that take a maximum number of
;;; named parameters.  Those are all the directives except the
;;; call-function directive.
(defclass named-parameters-directive (directive) ())

;;; Split a control string into its components.  Each component is
;;; either a string to be printed as it is, or a directive.  The list
;;; of components will never contain two consecutive strings.
(defun split-control-string (control-string)
  (loop with start = 0
        with end = (length control-string)
        while (< start end)
        collect (let ((tilde-position (position #\~ control-string :start start)))
                  (cond ((null tilde-position)
                         ;; No tilde was found.  The rest of the control string
                         ;; is just a string to be printed.
                         (prog1 (subseq control-string start end)
                           (setf start end)))
                        ((> tilde-position start)
                         ;; A tilde was found, but it is not in the
                         ;; start position.  A prefix of the control
                         ;; string is therefore a string to be
                         ;; printed.
                         (prog1 (subseq control-string start tilde-position)
                           ;; Make sure we find the tilde at the start position
                           ;; in the next iteration.
                           (setf start tilde-position)))
                        (t
                         ;; We found a tilde in the start position, so we have
                         ;; a directive.
                         (multiple-value-bind (directive-character
                                               parameters
                                               colonp
                                               at-signp
                                               end-of-directive-position)
                             (parse-format-directive control-string tilde-position)
                           (prog1 (make-instance 'directive
                                    :control-string control-string
                                    :start tilde-position
                                    :end end-of-directive-position
                                    :directive-character (char-upcase directive-character)
                                    :given-parameters parameters
                                    :colonp colonp
                                    :at-signp at-signp)
                             (setf start end-of-directive-position))))))))


;;; Return the name of a subclass to be used for a particular
;;; directive.  Each particular directive subclass must be accompanied
;;; by an eql-specialized method on this generic function.
(defgeneric directive-subclass-name (directive-character directive))

;;; For the default case, signal an error
(defmethod directive-subclass-name (directive-character directive)
  (error 'unknown-directive-character
         :directive directive))

;;; Given a name of a type of a directive, return a list of parameter
;;; specifiers for that type of directive.  Each type of directive
;;; should supply an eql specialized method for this generic function.
(eval-when (:compile-toplevel :load-toplevel)
  (defgeneric parameter-specs (directive-name)))

;;; A macro that helps us define directives. It takes a directive
;;; character, a directive name (to be used for the class) and a body
;;; in the form of a list of parameter specifications.  Each parameter
;;; specification is a list where the first element is the name of the
;;; parameter, and the remaining elemnts are keyword/value pairs.
;;; Currently, the only keywords allowed are :type and
;;; :default-value.
(defmacro define-directive (character name superclasses parameters &body slots)
  `(progn
     (defmethod directive-subclass-name
         ((char (eql ,(char-upcase character))) directive)
       (declare (ignore directive))
       ',name)

     (eval-when (:compile-toplevel :load-toplevel)
       (defmethod parameter-specs ((directive-name (eql ',name)))
         ',(loop for parameter in parameters
                 collect (if (getf (cdr parameter) :default-value)
                             parameter
                             (cons (car parameter)
                                   (list* :default-value nil (cdr parameter)))))))

     (defclass ,name ,superclasses
       (,@(loop for parameter in parameters
                collect `(,(car parameter)
                           :initform nil
                           :reader
                           ,(car parameter)))
          ,@slots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking syntax, interpreting, and compiling directives.

;;; For certain common types used by FORMAT, return a string
;;; explaining in English what the type means.  For other
;;; types, return a string "an object of type <type>"
(defun type-name (type)
  (cond ((symbolp type)
         (case type
           (integer "an integer")
           (character "a character")
           (list "a list")
           (t (format nil "an object of type ~s" type))))
        ((and (consp type) (eq (car type) 'integer))
         (case (length type)
           (1 "an integer")
           (2 (case (second type)
                (0 "a nonnegative integer")
                (1 "a strictly positive integer")
                (t (format nil "an integer greater than or equal to ~d" (second type)))))
           (3 (format nil "an integer between ~d and ~d" (second type) (third type)))
           (t (format nil "an object of type ~s" type))))
        (t (format nil "an object of type ~s" type))))

;;; Specialize a directive according to a particular directive
;;; character.
(defun specialize-directive (directive)
  (change-class directive (directive-subclass-name (directive-character directive) directive)))

;;; Check the syntax of a directive.
(defgeneric check-directive-syntax (directive)
  (:method-combination progn :most-specific-last))

(defmethod check-directive-syntax progn (directive)
  (with-accessors ((given-parameters given-parameters))
    directive
    (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
      ;; When a parameter was explicitly given, check that
      ;; what was given does not have an incompatible type
      ;; with respect to the default value of the corresponding
      ;; slot, and assign the explicitly given value to
      ;; the slot.
      (let ((parameter-number 1))
        (mapc (lambda (parameter-spec parameter-value)
                (unless (or (eq parameter-value '|#|)
                            (eq parameter-value 'V))
                  (unless
                      (or
                       ;; Either a parameter was not supplied, but it has a
                       ;; default value
                       (and (null parameter-value)
                            (not (null (getf (cdr parameter-spec) :default-value))))
                       ;; Or else it was supplied, and it is of the right type.
                       (typep parameter-value (getf (cdr parameter-spec) :type)))
                    (error 'parameter-type-error
                           :expected-type
                           (getf (cdr parameter-spec) :type)
                           :datum parameter-value)))
                (setf (slot-value directive (car parameter-spec))
                      parameter-value)
                (incf parameter-number))
        parameter-specs
        given-parameters)))))

(defmethod check-directive-syntax progn ((directive named-parameters-directive))
  (with-accessors ((given-parameters given-parameters))
    directive
    (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
      ;; Check that the number of parameters given is no more than
      ;; what this type of directive allows.
      (when (> (length given-parameters) (length parameter-specs))
        (error 'too-many-parameters
               :directive directive
               :at-most-how-many (length parameter-specs)
               :how-many-found (length given-parameters))))))

;;; Runtime environment

;;; During runtime, this variable is bound to a stream to which
;;; all the output goes.
(defvar *destination*)

;;; A vector of all the arguments that were passed to this
;;; invocation of FORMAT.
(defvar *arguments*)

;;; An index into the vector of arguments indicating the next
;;; argument to treat.
(defvar *next-argument-pointer*)

;;; A tag for catch/throw to use by the ~^ directive
(defvar *catch-tag*)

(defun compute-parameter-value (directive parameter-spec)
  (let* ((parameter-name (car parameter-spec))
         (compile-time-value (funcall parameter-name directive)))
    (cond ((null compile-time-value)
           ;; The parameter was not given at all in the format control
           ;; string, neither as a constant value, nor as a value to
           ;; be acquired at runtime (# or V).  We must use a default
           ;; value if it has any.
           (getf (cdr parameter-spec) :default-value))
          ((eq compile-time-value 'V)
           ;; The parameter was given the explicit value V in the
           ;; format control string, meaning we use the next argument
           ;; to acquire the value of the parameter.  We must test
           ;; that there are more arguments, consume the next one, and
           ;; check that the type of the argument acquired is correct.
           (when (>= *next-argument-pointer*
                     (length *arguments*))
             (error 'no-more-arguments))
           (let ((argument (aref *arguments*
                                 *next-argument-pointer*)))
             (incf *next-argument-pointer*)
             (unless (typep argument (getf (cdr parameter-spec) :type))
               (error 'argument-type-error
                      :expected-type
                      (getf (cdr parameter-spec) :type)
                      :datum
                      argument))
             argument))
          ((eq compile-time-value '|#|)
           ;; The parameter was given the explicit value # in the
           ;; format control string, meaning we use the number of
           ;; remaining arguments as the value of the parameter.
           (let ((number-of-remaining-arguments
                  (- (length *arguments*) *next-argument-pointer*)))
             (unless (typep number-of-remaining-arguments
                            (getf (cdr parameter-spec) :type))
               (error 'argument-type-error
                      :expected-type
                      (getf (cdr parameter-spec) :type)
                      :datum
                      number-of-remaining-arguments))
             number-of-remaining-arguments))
          (t
           ;; The parameter was given an explicit value (number or
           ;; character) in the format control string, and this is the
           ;; value we want.
           compile-time-value))))

;;; The directive interpreter.

;;; DIRECTIVE is an instance of a subclass of the DIRECTIVE class
;;; describing the directive.
(defgeneric interpret-format-directive (directive))

(defmethod interpret-format-directive (directive)
  (error 'unknown-format-directive
         :control-string (control-string directive)
         :tilde-position (start directive)
         :index (1- (end directive))))

(defmacro define-format-directive-interpreter (class-name &body body)
  `(defmethod interpret-format-directive ((directive ,class-name))
     (with-accessors ((control-string control-string)
                      (start start)
                      (end end)
                      (colonp colonp)
                      (at-signp at-signp))
       directive
       (let ,(loop for parameter-spec in (parameter-specs class-name)
                   collect `(,(car parameter-spec)
                              (compute-parameter-value directive ',parameter-spec)))
         ,@body))))

(defun consume-next-argument (type)
  (when (>= *next-argument-pointer* (length *arguments*))
    (error 'no-more-arguments))
  (let ((arg (aref *arguments* *next-argument-pointer*)))
    (incf *next-argument-pointer*)
    (unless (typep arg type)
      (error 'argument-type-error
             :expected-type type
             :datum arg))
    arg))

;;; The directive compiler.
(defgeneric compile-format-directive (directive))

(defmacro define-format-directive-compiler (class-name &body body)
  `(defmethod compile-format-directive ((directive ,class-name))
     (with-accessors ((control-string control-string)
                      (start start)
                      (end end)
                      (colonp colonp)
                      (at-signp at-signp)
                      (given-parameters given-parameters)
                      ,@(loop for parameter-spec in (parameter-specs class-name)
                              collect `(,(car parameter-spec) ,(car parameter-spec))))
       directive
       ,@body)))

(defun compile-time-value (directive slot-name)
  (let ((value (slot-value directive slot-name)))
    (cond ((or (eq value '|#|) (eq value 'V))
           nil)
          ((null value)
           (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
             (getf (cdr (find slot-name parameter-specs :key #'car)) :default-value)))
          (t
           value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code for individual directives


;;; Mixin class for directives that take no modifiers
(defclass no-modifiers-mixin () ())

;;; Signal an error of a modifier has been given for such a directive.
(defmethod check-directive-syntax progn ((directive no-modifiers-mixin))
  (with-accessors ((colonp colonp)
                   (at-signp at-signp)
                   (control-string control-string)
                   (end end))
    directive
    (when (or colonp at-signp)
      (error 'directive-takes-no-modifiers
             :directive directive))))


;;; Mixin class for directives that take only colon modifiers
(defclass only-colon-mixin () ())

;;; Signal an error of an at-sign has been given for such a directive.
(defmethod check-directive-syntax progn ((directive only-colon-mixin))
  (with-accessors ((at-signp at-signp)
                   (control-string control-string)
                   (end end))
    directive
    (when at-signp
      (error 'directive-takes-only-colon
             :directive directive))))


;;; Mixin class for directives that take only at-sign modifiers
(defclass only-at-sign-mixin () ())

;;; Signal an error of a colon has been given for such a directive.
(defmethod check-directive-syntax progn ((directive only-at-sign-mixin))
  (with-accessors ((colonp colonp)
                   (control-string control-string)
                   (end end))
    directive
    (when colonp
      (error 'directive-takes-only-at-sign
             :directive directive))))

;;; Mixin class for directives that take at most one modifier
(defclass at-most-one-modifier-mixin () ())

;;; Signal an error if both modifiers have been given for such a directive.
(defmethod check-directive-syntax progn ((directive at-most-one-modifier-mixin))
  (with-accessors ((colonp colonp)
                   (at-signp at-signp)
                   (control-string control-string)
                   (end end))
    directive
    (when (and colonp at-signp)
      (error 'directive-takes-at-most-one-modifier
             :directive directive))))


;;; Mixin class for structured directives
(defclass structured-directive-mixin ()
  ((%items :initarg :items :reader items)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2 Radix control

(defun print-radix-arg (radix colonp at-signp mincol padchar commachar comma-interval)
  (let ((argument (consume-next-argument t)))
    (if (not (integerp argument))
        (let ((*print-base* 10))
          (format *destination* "~a" argument))
        (let* ((string (let ((*print-base* radix))
                         (princ-to-string (abs argument))))
               (comma-length (if colonp
                                 (max 0 (floor (1- (length string)) comma-interval))
                                 0))
               (sign-length (if (or at-signp (minusp argument)) 1 0))
               (total-length (+ (length string) comma-length sign-length))
               (pad-length (max 0 (- mincol total-length))))
          ;; Print the padding.
          (loop repeat pad-length
                do (write-char padchar *destination*))
          ;; Possibliy print a sign.
          (cond ((minusp argument)
                 (write-char #\- *destination*))
                (at-signp
                 (write-char #\+ *destination*))
                (t nil))
          ;; Print the string in reverse order
          (loop for index downfrom (1- (length string)) to 0
                for c across string
                do (write-char c *destination*)
                do (when (and colonp
                              (plusp index)
                              (zerop (mod index comma-interval)))
                     (write-char commachar *destination*)))))))

;;; 22.3.4 Printer operations

(defun print-a-or-s (raw-output at-signp mincol colinc minpad padchar)
  (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
    (if at-signp
        (progn (loop repeat pad-length do (write-char padchar *destination*))
               (write-string raw-output *destination*))
        (progn (write-string raw-output *destination*)
               (loop repeat pad-length do (write-char padchar *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main entry point

(defun structure-items (items end)
  (loop with result = '()
        with first = (car items)
        do (cond ((null items)
                  (if (null end)
                      (return (values (coerce (nreverse result) 'vector)
                                      '()))
                      (error 'unmatched-directive
                             :directive first
                             :control-string (control-string first)
                             :tilde-position (start first))))
                 ((stringp (car items))
                  (push (pop items) result))
                 ((find (directive-character (car items))
                        ">)}]")
                  (if (eql (directive-character (car items)) end)
                      (progn (push (pop items) result)
                             (return (values (coerce (nreverse result) 'vector)
                                             items)))
                      (error 'nesting-violation
                             :directive (car items))))
                 ((find (directive-character (car items))
                        "<({[")
                  (let ((item (pop items)))
                    (multiple-value-bind (nested-items rest)
                        (structure-items items
                                         (ecase (directive-character item)
                                           (#\< #\>) (#\( #\)) (#\{ #\}) (#\[ #\])))
                      (setf items rest)
                      (ecase (directive-character item)
                        (#\< (if (colonp (aref nested-items (1- (length nested-items))))
                                 (change-class item 'logical-block-directive
                                               :items nested-items)
                                 (change-class item 'justification-directive
                                               :items nested-items)))
                        (#\( (change-class item 'case-conversion-directive
                                           :items nested-items))
                        (#\{ (change-class item 'iteration-directive
                                           :items nested-items))
                        (#\[ (change-class item 'conditional-directive
                                           :items nested-items)))
                      (check-directive-syntax item)
                      (push item result))))
                 (t
                  (let ((item (pop items)))
                    (specialize-directive item)
                    (check-directive-syntax item)
                    (push item result))))))

(defun interpret-items (items)
  (loop for item across items
        do (if (stringp item)
               (write-string item *destination*)
               (interpret-format-directive item))))

;;; The reason we define this function is that the ~? directive
;;; (recursive processing), when a @ modifier is used, reuses
;;; the arguments of the parent control string, so we need
;;; to call a version of format that doesn't initialize the
;;; *arguments* runtime environment variable.
(defun format-with-runtime-arguments (destination control-string)
  (flet ((format-aux (stream items)
           ;; interpret the control string in a new environment
           (let ((*destination* stream)
                 ;; We are at the beginning of the argument vector.
                 (*next-argument-pointer* 0)
                 ;; Any unique object will do.
                 (*catch-tag* (list nil)))
             (catch *catch-tag*
               (interpret-items items)))))
    (let ((items (structure-items (split-control-string control-string) nil)))
      (cond ((or (streamp destination)
                 (and (stringp destination)
                      (array-has-fill-pointer-p destination)))
             (format-aux destination items))
            ((null destination)
             (with-output-to-string (stream)
                                    (format-aux stream items)))
            ((eq destination t)
             (format-aux *standard-output* items))
            (t
             (error 'invalid-destination
                    :destination destination))))))

(defun format (destination control-string &rest args)
  (let (;; initialize part of the runtime environment here
        (*arguments* (coerce args 'vector)))
    (format-with-runtime-arguments destination control-string)))
