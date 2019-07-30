(asdf:load-asd (truename "kishtml.asd") :name "kishtml")
(asdf:load-asd (truename "kishtml-tests.asd") :name "kishtml-tests")
(asdf:load-system :kishtml-tests)

(kishtml-tests:test-kishtml)
