(cl:in-package #:asdf-user)

(defsystem :sicl-format
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "format")
   ;; 22.3.1 Basic output
   (:file "c-directive")
   (:file "percent-directive")
   (:file "ampersand-directive")
   (:file "vertical-bar-directive")
   (:file "tilde-directive")
   ;; 22.3.2 Radix control
   (:file "r-directive")
   (:file "d-directive")
   (:file "b-directive")
   (:file "o-directive")
   (:file "x-directive")
   ;; 22.3.3 Floating-point printers
   (:file "f-directive")
   (:file "e-directive")
   (:file "g-directive")
   (:file "monetary-floating-point-directive")
   ;; 22.3.4 Printer operations
   (:file "a-directive")
   (:file "s-directive")
   (:file "w-directive")
   ;; 22.3.5 Pretty printer operations
   (:file "underscore-directive")
   (:file "logical-block-directive")
   (:file "i-directive")
   (:file "call-function-directive")
   (:file "layout-control-directieve")
   (:file "tabulate-directive")
   (:file "justification-directive")
   (:file "greater-than-directive")
   ;; 22.3.7 Control-flow operations
   (:file "go-to-directive")
   (:file "conditional-directive")
   (:file "right-bracket-directive")
   (:file "iteration-directive")
   (:file "right-brace-directive")
   (:file "recursive-processing-directive")
   ;; 22.3.8 Miscellaneous operations
   (:file "case-conversion-directive")
   (:file "right-paren-directive")
   (:file "plural-directive")
   ;; 22.3.9 Miscellaneous pseudo-operations
   (:file "semicolon-directive")
   (:file "circumflex-directive")
   (:file "newline-directive")
   (:file "control-string-compiler")
   (:file "format-define-compiler-macro")))
