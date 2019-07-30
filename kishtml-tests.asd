(defsystem #:kishtml-tests
  :description "Test Suite of Keep It Simple HTML."
  :author "Donatas Petrauskas <donatas.petr@gmail.com>"
  :licence "All right reserved."
  :depends-on (:fiveam :kishtml)
  :components ((:module "t"
                :serial t
                :components ((:file "kishtml")))))
