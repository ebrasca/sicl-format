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
;; (asdf:defsystem #:sicl-format
;;   :description "Describe sicl-format here"
;;   :author "Your Name <your.name@example.com>"
;;   :license  "Specify license here"
;;   :version "0.0.1"
;;   :serial t
;;   :components ((:file "package")
;;                (:file "sicl-format")))
