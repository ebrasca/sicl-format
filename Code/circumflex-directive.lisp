;;; 22.3.9.2 ~^ Escape upward directive.

(cl:in-package #:sicl-format)

(define-directive #\^ circumflex-directive (named-parameters-directive)
    ((p1 :type integer)
     (p2 :type integer)
     (p3 :type integer)))

(defmethod check-directive-syntax progn
  ((directive circumflex-directive))
  (let ((parameters (given-parameters directive)))
    (when (and (second parameters) (not (first parameters)))
      (error 'parameter-omitted
             :parameter1 1
             :parameter2 2))
    (when (and (third parameters) (not (second parameters)))
      (error 'parameter-omitted
             :parameter2 2
             :parameter3 3))))

(define-format-directive-interpreter circumflex-directive
  (let ((parameters (given-parameters directive)))
    (cond ((not (first parameters))
           (throw *catch-tag* nil))
          ((not (second parameters))
           (when (zerop p1)
             (throw *catch-tag* nil)))
          ((not (third parameters))
           (when (= p1 p2)
             (throw *catch-tag* nil)))
          (t
           (when (<= p1 p2 p3)
             (throw *catch-tag* nil))))))

(define-format-directive-compiler circumflex-directive
  (let ((parameters (given-parameters directive)))
    (cond ((not (first parameters))
           `(throw *catch-tag* nil))
          ((not (second parameters))
           `(when (zerop p1)
              (throw *catch-tag* nil)))
          ((not (third parameters))
           `(when (= p1 p2)
              (throw *catch-tag* nil)))
          (t
           `(when (<= p1 p2 p3)
              (throw *catch-tag* nil))))))
