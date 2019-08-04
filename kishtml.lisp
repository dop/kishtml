(defpackage #:kishtml
  (:use #:cl)
  (:export #:->string #:->fragment #:*doctype*)
  (:import-from #:html-entities #:encode-entities))

(in-package #:kishtml)

(defvar *doctype* :html5)

(defvar *void-elements*
  '(:area :base :br :col :command :embed :hr :img :input :keygen :link :meta :param :source :track :wbr))

(defun concat (&rest strings)
  (with-output-to-string (s)
    (loop :for str :in strings :do (write-string str s))))

(defun mapconcat (fn &rest lists)
  (apply #'concat (apply #'mapcar fn lists)))

(defun void-element-p (tag)
  (member tag *void-elements*))

(defun pairp (obj)
  (and (consp obj)
       (let ((a (car obj))
             (b (cdr obj)))
         (and (atom a) (atom b) b))))

(defun attr->string (name value)
  (cond
    ((stringp value) (format nil "~A=\"~A\"" name (encode-entities value)))
    ((numberp value) (format nil "~A=~A" name value))
    (value (format nil "~A" name))))

(defun attrs->string (attrs)
  (when (consp attrs)
    (with-output-to-string (*standard-output*)
      (loop :for (name . value) :in attrs
            :when value
              :do (write-char #\space)
                  (write-string (attr->string (symbol-name name) value))))))

(defun tag->string (tag &optional attrs children)
  (declare (type symbol tag)
           (type list attrs children))
  (concatenate 'string "<" (symbol-name tag) (attrs->string attrs)
               (if (void-element-p tag)
                   ">"
                   (concatenate 'string ">" (mapconcat #'fragment->string children) "</" (symbol-name tag) ">"))))

(defun fragment->string (obj)
  (cond
    ((stringp obj)
     (encode-entities obj))
    ((atom obj)
     (write-to-string obj))
    ((consp obj)
     (let ((tag (car obj))
           (attrs (cadr obj)))
       (cond ((and (listp attrs) (every #'pairp attrs))
              (tag->string tag attrs (cddr obj)))
             (t
              (tag->string tag () (cdr obj))))))))

(defun ->fragment (&rest objs)
  (let ((*doctype* nil))
    (apply #'->string objs)))

(defun ->string (&rest objs)
  (with-output-to-string (*standard-output*)
    (case *doctype*
      (:html5 (write-string "<!DOCTYPE html>")))
    (loop :for obj :in objs
          :do (write-string (fragment->string obj)))))
