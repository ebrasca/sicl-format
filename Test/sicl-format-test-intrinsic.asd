(cl:in-package #:asdf-user)

(defsystem :sicl-format-test-intrinsic
  :depends-on (:lisp-unit :sicl-format-intrinsic)
  :serial t
  :components
  ((:file "packages-intrinsic")
   (:file "format-test")))
