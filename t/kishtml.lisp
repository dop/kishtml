(defpackage #:kishtml-tests
  (:use :cl :fiveam :kishtml)
  (:export #:test-kishtml))

(in-package #:kishtml-tests)

(def-suite kishtml-tests
  :description "Suite of KisHTML tests.")

(defun test-kishtml ()
  (run! 'kishtml-tests))

(in-suite kishtml-tests)

(defun link (url text)
  `(:a ((:class . "link") (:href . ,url))
       (:span ,text)))

(defun page (title &rest content)
  `(:html
    (:head
     (:meta ((:name . "charset") (:value . "utf-8")))
     (:title ,title)
     (:link ((:rel . "stylesheet") (:href . "style.css") (:type . "text/css")))
     (:script ((:type . "text/javascript") (:src . "scripts.js"))))
    (:body
     (:h1 ,title)
     ,@content)))

(defun page2 (title &rest content)
  `(:html
    (:head
     (:meta :name "charset" :value "utf-8")
     (:title ,title)
     (:link :rel "stylesheet" :href "style.css" :type "text/css")
     (:script :type "text/javascript" :src "scripts.js"))
    (:body
     (:h1 ,title)
     ,@content)))

(test ->fragment
  (is (equalp "" (->fragment nil)))
  (is (equalp "<hr>" (->fragment '(:hr))))
  (is (equalp "<br>" (->fragment '(:br))))
  (is (equalp "<input type=\"text\" maxlength=20 tabindex=1 name=\"first_name\" required>"
              (->fragment '(:input ((type . "text") (maxlength . 20) (tabindex . 1) (name . "first_name") (required . t))))))
  (is (equalp "<div class=\"sep\"></div>" (->fragment '(:div ((class . "sep"))))))
  (is (equalp "<a href=\"google.com\">Search</a>"
              (->fragment '(:a ((:href . "google.com")) "Search"))))
  (is (equalp "<a href=\"google.com\"><span>Click Me!</span></a>"
              (->fragment '(:a ((:href . "google.com")) (:span nil "Click Me!")))))
  (is (equalp "<head><title>Blog</title></head>"
              (->fragment '(:head () (:title () "Blog")))))
  (is (equalp "<body><h1>My Blog</h1><p>Some text <a href=\"\">a link</a></p></body>"
              (->fragment '(:body () (:h1 () "My Blog") (:p () "Some text " (:a ((:href . "")) "a link"))))))
  (is (equalp "<h1>&lt;title&gt;</h1><a href=\"http://search.com/?q=&quot;hello world&quot;\">link</a>"
              (->fragment '(:h1 "<title>") '(:a ((:href . "http://search.com/?q=\"hello world\"")) "link")))))

(test ->string
  (is (equalp "<!doctype html>
<body><h1>My Blog</h1><p>Some text <a href=\"\">a link</a></p></body>"
              (->string '(:body (:h1 "My Blog") (:p "Some text " (:a ((:href . "")) "a link"))))))
  (is (equalp "<!DOCTYPE html>
<HTML LANG=\"en\"></HTML>"
              (->string '(:html ((:lang . "en"))))))
  (let ((expected-page-output "<!DOCTYPE html>
<HTML><HEAD><META NAME=\"charset\" VALUE=\"utf-8\"><TITLE>Table of Contents</TITLE><LINK REL=\"stylesheet\" HREF=\"style.css\" TYPE=\"text/css\"><SCRIPT TYPE=\"text/javascript\" SRC=\"scripts.js\"></SCRIPT></HEAD><BODY><H1>Table of Contents</H1><UL><LI CLASS=\"item\"><A CLASS=\"link\" HREF=\"/story-1.html\"><SPAN>Story 1</SPAN></A></LI><LI CLASS=\"item\"><A CLASS=\"link\" HREF=\"/story-2.html\"><SPAN>Story 2</SPAN></A></LI></UL></BODY></HTML>"))
    (is (equalp
         expected-page-output
         (let ((links '(("Story 1" . "/story-1.html")
                        ("Story 2" . "/story-2.html"))))
           (->string
            (page "Table of Contents"
                  `(:ul ,@(loop :for (title . url) :in links
                                :collect `(:li ((:class . "item")) ,(link url title)))))))))
    (is (equalp
         expected-page-output
         (let ((links '(("Story 1" . "/story-1.html")
                        ("Story 2" . "/story-2.html"))))
           (->string
            (page2 "Table of Contents"
                   `(:ul ,@(loop :for (title . url) :in links
                                 :collect `(:li :class "item" ,(link url title)))))))))))

(test *doctype*
  (let ((*doctype* nil))
    (is (equalp "<html></html>" (->string '(:html)))))
  (let ((*doctype* :html5))
    (is (equalp "<!DOCTYPE html>
<html><head><meta name=\"charset\" value=\"utf-8\"></head></html>"
                (->string '(:html (:head (:meta :name "charset" :value "utf-8")))))))
  (let ((*doctype* :xhtml))
    (is (equalp "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html><head><meta name=\"charset\" value=\"utf-8\"/></head></html>"
                (->string '(:html (:head (:meta :name "charset" :value "utf-8"))))))))
