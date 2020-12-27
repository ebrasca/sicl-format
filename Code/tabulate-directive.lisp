;;; 22.3.6.1 ~TAB Tabulate directive.

(cl:in-package #:sicl-format)

(define-directive #\t tabulate-directive (named-parameters-directive)
    ((colnum :type (integer 1) :default-value 1)
     (colinc :type (integer 1) :default-value 1)))

(define-format-directive-interpreter tabulate-directive
  (if (colonp
       (pprint-tab (if at-signp :section-relative :section)
                   colnum colinc *destination*))
      ;; Thanks to Brian Mastenbrook for suggesting this solution.
      (let ((*print-level* nil))
        (pprint-logical-block (*destination* nil)
                              (pprint-tab (if at-signp :line-relative :line)
                                          colnum colinc *destination*)))))

(define-format-directive-compiler tabulate-directive
  (if (colonp
       `(pprint-tab ,(if at-signp :section-relative :section)
                    colnum colinc *destination*))
      ;; Thanks to Brian Mastenbrook for suggesting this solution.
      `(let ((*print-level* nil))
         (pprint-logical-block (*destination* nil)
                               (pprint-tab ,(if at-signp :line-relative :line)
                                           colnum colinc *destination*)))))
