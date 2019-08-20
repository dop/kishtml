;; Usual Lisp comments are allowed here

(in-package :asdf-user)

(defsystem #:kishtml
  :description "Keep It Simple HTML."
  :version "0.0.1"
  :author "Donatas Petrauskas <donatas.petr@gmail.com>"
  :licence "All right reserved."
  :depends-on (:html-entities)
  :components ((:file "kishtml")))

(defsystem #:kishtml/test
  :description "Test Suite of Keep It Simple HTML."
  :author "Donatas Petrauskas <donatas.petr@gmail.com>"
  :licence "All right reserved."
  :depends-on (:fiveam :kishtml)
  :components ((:module "t"
                :serial t
                :components ((:file "kishtml")))))
