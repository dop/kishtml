;; Usual Lisp comments are allowed here

(defsystem #:kishtml
  :description "Keep It Simple HTML."
  :version "0.0.1"
  :author "Donatas Petrauskas <donatas.petr@gmail.com>"
  :licence "All right reserved."
  :depends-on (:trivia)
  :components ((:file "kishtml")))
