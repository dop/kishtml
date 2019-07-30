(defpackage #:kishtml-tests
  (:use :cl :fiveam :kishtml)
  (:export #:test-kishtml))

(in-package #:kishtml-tests)

(def-suite kishtml-tests
  :description "Suite of KisHTML tests.")

(defun test-kishtml ()
  (run! 'kishtml-tests))

(in-suite kishtml-tests)

(test attr->string
  (is (null (attr->string "" nil)))
  (is (null (attr->string "a" nil)))
  (is (equalp "a=1" (attr->string "a" 1)))
  (is (equalp "a=\"b\"" (attr->string "a" "b")))
  (is (equalp "a" (attr->string "a" t))))

(test attrs->string
  (is (null (attrs->string '())))
  (is (equalp "a=1" (attrs->string '((a . 1)))))
  (is (equalp "a=1" (attrs->string '((:a . 1)))))
  (is (equalp "a=1" (attrs->string '(("a" . 1)))))
  (is (equalp "a=1 b c=\"x\"" (attrs->string '((a . 1) (b . t) (c . "x") (d . nil))))))

(test tag->string
  (is (equalp "<hr>" (tag->string :hr)))
  (is (equalp "<br>" (tag->string :br)))
  (is (equalp "<div class=\"sep\"/>" (tag->string :div '((class . "sep")))))
  (is (equalp "<a href=\"google.com\">Search</a>"
              (tag->string :a '((:href . "google.com")) '("Search"))))
  (is (equalp "<a href=\"google.com\"><span>Click Me!</span></a>"
              (tag->string :a '((:href . "google.com")) '((:span nil "Click Me!")))))

  (is (equalp "<head><title>Blog</title></head>"
              (tag->string :head () '((:title () "Blog"))))))

(test ->string
  (is (equalp "<!doctype html><body><h1>My Blog</h1><p>Some text <a href=\"\">a link</a></p></body>"
              (->string '(:doctype (:body () (:h1 () "My Blog") (:p () "Some text " (:a ((:href . "")) "a link")))))))
  (is (equalp "<!doctype html><body><h1>My Blog</h1><p>Some text <a href=\"\">a link</a></p></body>"
              (->string '(:doctype (:body (:h1 "My Blog") (:p "Some text " (:a ((:href . "")) "a link")))))))

  (is (equalp "<!DOCTYPE html><HTML LANG=\"en\"><HEAD><META NAME=\"charset\" VALUE=\"utf-8\"/><TITLE>Donatas' Blog</TITLE><LINK REL=\"stylesheet\" HREF=\"style.css\" TYPE=\"text/css\"/><SCRIPT TYPE=\"text/javascript\" SRC=\"scripts.js\"/></HEAD><BODY><H1>Donatas' Blog</H1><P>This is my blog</P></BODY></HTML>"
              (->string '(:doctype
                          (:html ((:lang . "en"))
                           (:head
                            (:meta ((:name . "charset") (:value . "utf-8")))
                            (:title "Donatas' Blog")
                            (:link ((:rel . "stylesheet") (:href . "style.css") (:type . "text/css")))
                            (:script ((:type . "text/javascript") (:src . "scripts.js"))))
                           (:body
                            (:h1 "Donatas' Blog")
                            (:p "This is my blog"))))))))
