(defpackage #:kishtml
  (:use #:cl #:trivia)
  (:export #:->string #:attr->string #:attrs->string #:tag->string))

(in-package #:kishtml)

(defun concat (&rest strings)
  (apply #'concatenate 'string strings))

(defun ->string (doc &optional &key doctype)
  (with-output-to-string (*standard-output*)
    (when doctype (write-string "<!DOCTYPE html>"))
    (write-string (apply #'concat (mapcar #'fragment->string doc)))))

(defun pairp (obj)
  (and (consp obj)
       (let ((a (car obj))
             (b (cdr obj)))
         (and (atom a)
              (atom b)
              b))))

(defun fragment->string (fragment)
  (match fragment
    ((list* tag attrs body)
     (cond ((and (listp attrs) (every #'pairp attrs))
            (tag->string tag attrs body))
           (t
            (tag->string tag () (cons attrs body)))))
    (_ fragment)))

(defun attr-name (name)
  (cond
    ((symbolp name) (symbol-name name))
    (t name)))

(defun attrs->string (attrs)
  (when (consp attrs)
    (format nil "~{~A~^ ~}"
            (loop :for (name . value) :in attrs
                  :when value
                  :collect (attr->string (attr-name name) value)))))

(defun attr->string (name value)
  (cond
    ((stringp value) (format nil "~A=\"~A\"" name value))
    ((numberp value) (format nil "~A=~A" name value))
    (value (format nil "~A" name))))

(defun tag-name (tag)
  (symbol-name tag))

(defun wrap-if (value &optional (left " ") (right ""))
  (if (zerop (length value))
      ""
      (concatenate 'string left value right)))

(defparameter *open-tags*
  '(:hr :br :input :img :meta))

(defun tag->string (tag &optional attrs children)
  (declare (type symbol tag)
           (type list attrs children))
  (let ((self-closing-p (member tag *open-tags*)))
    (concatenate 'string "<" (tag-name tag) (wrap-if (attrs->string attrs))
                 (if self-closing-p
                     ">"
                 (if children
                     (concatenate 'string ">" (->string children) "</" (tag-name tag) ">")
                     "/>")))))
