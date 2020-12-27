;;; The symbols that are shadowed from the COMMON-LISP package
;;; are also symbols that we want to export.  To avoid repeating
;;; that list of symbols, we use the reader macros #= and ##.
;;; It is interesting to note that we would normally write
;;; (:shadow <string1> <string2> ...), but in order to put a
;;; reader label on the list (<string1> <string2> ...) we need to
;;; express that as (:shadow . (<string1> <string2> ...)) instead.
(defpackage #:sicl-format
    (:use #:cl)
  (:export #:format))
