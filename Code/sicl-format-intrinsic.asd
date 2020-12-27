(cl:in-package #:asdf-user)

(defsystem :sicl-format-intrinsic
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages-intrinsic")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "format")
   (:file "control-string-compiler")
   (:file "format-define-compiler-macro")))
