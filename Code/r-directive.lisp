;;; 22.3.2.1 ~r Radix directive.

(cl:in-package #:sicl-format)

(define-directive #\r r-directive (named-parameters-directive)
    ((radix :type (integer 2 36) :default-value nil)
     (mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

;;; Print an integer as roman numerals to the stream.
;;; The integer must be strictly greater than zero,
;;; and strictly less than 4000.
(defun print-as-roman (integer stream)
  (multiple-value-bind (thousands rest) (floor integer 1000)
    (loop repeat thousands
          do (write-char #\M stream))
    (multiple-value-bind (hundreds rest) (floor rest 100)
      (princ (case hundreds
               (0 "") (1 "C") (2 "CC") (3 "CCC") (4 "CD")
               (5 "D" ) (6 "DC") (7 "DCC") (8 "DCCC") (9 "CM"))
             stream)
      (multiple-value-bind (tenths rest) (floor rest 10)
        (princ (case tenths
                 (0 "") (1 "X") (2 "XX") (3 "XXX") (4 "XL")
                 (5 "L" ) (6 "LX") (7 "LXX") (8 "LXXX") (9 "XC"))
               stream)
        (princ (case rest
                 (0 "") (1 "I") (2 "II") (3 "III") (4 "IV")
                 (5 "V" ) (6 "VI") (7 "VII") (8 "VIII") (9 "IX"))
               stream)))))

;;; Print an integer as old roman numerals to the stream.
;;; The integer must be strictly greater than zero,
;;; and strictly less than 4000.
(defun print-as-old-roman (integer stream)
  (multiple-value-bind (thousands rest) (floor integer 1000)
    (loop repeat thousands
          do (write-char #\M stream))
    (multiple-value-bind (hundreds rest) (floor rest 100)
      (princ (case hundreds
               (0 "") (1 "C") (2 "CC") (3 "CCC") (4 "CCCC")
               (5 "D" ) (6 "DC") (7 "DCC") (8 "DCCC") (9 "DCCCC"))
             stream)
      (multiple-value-bind (tenths rest) (floor rest 10)
        (princ (case tenths
                 (0 "") (1 "X") (2 "XX") (3 "XXX") (4 "XXXX")
                 (5 "L" ) (6 "LX") (7 "LXX") (8 "LXXX") (9 "LXXXX"))
               stream)
        (princ (case rest
                 (0 "") (1 "I") (2 "II") (3 "III") (4 "IIII")
                 (5 "V" ) (6 "VI") (7 "VII") (8 "VIII") (9 "VIIII"))
               stream)))))

(defparameter *cardinal-ones*
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defparameter *cardinal-teens*
  #("ten" "eleven" "twelve" "thirteen" "fourteen"
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defparameter *cardinal-tens*
  #(nil nil "twenty" "thirty" "fourty"
    "fifty" "sixty" "seventy" "eighty" "ninety"))

(defparameter *groups-of-three*
  #(nil "thousand" "million" "billion" "trillion" "quadrillion"
    "quintillion" "sextillion" "septillion" "octillion" "nonillion"
    "decillion" "undecillion" "duodecillion" "tredecillion"
    "quattuordecillion" "quindecillion" "sexdecillion"
    "septendecillion" "octodecillion" "novemdecillion" "vigintillion"))

;;; print a cardinal number between 1 and 99
(defun print-cardinal-tenths (n stream)
  (cond ((< n 10)
         (princ (aref *cardinal-ones* n) stream))
        ((< n 20)
         (princ (aref *cardinal-teens* (- n 10)) stream))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (princ (aref *cardinal-tens* tens) stream)
           (unless (zerop ones)
             (princ "-" stream)
             (princ (aref *cardinal-ones* ones) stream))))))

;;; print a cardinal number between 1 and 999
(defun print-cardinal-hundreds (n stream)
  (cond ((< n 100)
         (print-cardinal-tenths n stream))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (princ (aref *cardinal-ones* hundreds) stream)
           (princ " hundred" stream)
           (unless (zerop rest)
             (princ " " stream)
             (print-cardinal-tenths rest stream))))))

;;; print a cardinal number n such that 0 < n < 10^65
(defun print-cardinal-non-zero (n stream magnitude)
  (multiple-value-bind (thousands rest) (floor n 1000)
    (unless (zerop thousands)
      (print-cardinal-non-zero thousands stream (1+ magnitude)))
    (unless (or (zerop thousands) (zerop rest))
      (princ " " stream))
    (unless (zerop rest)
      (print-cardinal-hundreds rest stream)
      (unless (zerop magnitude)
        (princ " " stream)
        (princ (aref *groups-of-three* magnitude) stream)))))

;;; print a cardinal number n such that - 10^65 < n < 10^65
(defun print-cardinal-number (n stream)
  (cond ((minusp n)
         (princ "negative " stream)
         (print-cardinal-non-zero (- n) stream 0))
        ((zerop n)
         (princ "zero" stream))
        (t
         (print-cardinal-non-zero n stream 0))))

(defparameter *ordinal-ones*
  #(nil "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"))

(defparameter *ordinal-teens*
  #("tenth" "eleventh" "twelvth" "thirteenth" "fourteenth"
    "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

(defparameter *ordinal-tens*
  #(nil nil "twentieth" "thirtieth" "fourtieth"
    "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

;;; print an ordinal number between 1 and 99
(defun print-ordinal-tenths (n stream)
  (cond ((< n 10)
         (princ (aref *ordinal-ones* n) stream))
        ((< n 20)
         (princ (aref *ordinal-teens* (- n 10)) stream))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (if (zerop ones)
               (princ (aref *ordinal-tens* tens) stream)
               (progn (princ (aref *cardinal-tens* tens) stream)
                      (princ "-" stream)
                      (princ (aref *ordinal-ones* ones) stream)))))))

;;; print an ordinal number n such that 0 < n < 1000
(defun print-ordinal-hundreds (n stream)
  (cond ((< n 100)
         (print-ordinal-tenths n stream))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (princ (aref *cardinal-ones* hundreds) stream)
           (princ " hundred" stream)
           (if (zerop rest)
               (princ "th" stream)
               (progn (princ " " stream)
                      (print-ordinal-tenths rest stream)))))))

;;; print an ordinal number n such that 0 < n < 10^65
(defun print-ordinal-non-zero (n stream)
  (multiple-value-bind (hundreds rest) (floor n 100)
    (cond ((zerop rest)
           ;; hudreds is nonzero
           (print-cardinal-non-zero n stream 0)
           (princ "th" stream))
          ((zerop hundreds)
           (print-ordinal-hundreds rest stream))
          (t
           ;; they are both nonzero
           (print-cardinal-non-zero (* 100 hundreds) stream 0)
           (princ " " stream)
           (print-ordinal-tenths rest stream)))))

;;; print an ordninal number n such that - 10^65 < n < 10^65
(defun print-ordinal-number (n stream)
  (cond ((minusp n)
         (princ "negative " stream)
         (print-ordinal-non-zero (- n) stream))
        ((zerop n)
         (princ "zeroth" stream))
        (t
         (print-ordinal-non-zero n stream))))

(define-format-directive-interpreter r-directive
  (cond ((not (null radix))
         (print-radix-arg radix colonp at-signp mincol padchar commachar comma-interval))
        ((and colonp at-signp)
         (print-as-old-roman (consume-next-argument '(integer 1))
                             *destination*))
        (at-signp
         (print-as-roman (consume-next-argument '(integer 1))
                         *destination*))
        (colonp
         (print-ordinal-number (consume-next-argument
                                `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                               *destination*))
        (t
         (print-cardinal-number (consume-next-argument
                                 `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                                *destination*))))

(define-format-directive-compiler r-directive
    (cond ((not (null radix))
           `(print-radix-arg radix ,colonp ,at-signp mincol padchar commachar comma-interval))
          ((and colonp at-signp)
           `(print-as-old-roman (consume-next-argument '(integer 1))
                                *destination*))
          (at-signp
           `(print-as-roman (consume-next-argument '(integer 1))
                            *destination*))
          (colonp
           `(print-ordinal-number (consume-next-argument
                                   `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                                  *destination*))
          (t
           `(print-cardinal-number (consume-next-argument
                                    `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                                   *destination*))))
