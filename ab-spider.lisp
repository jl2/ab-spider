;;;; ab-spider.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:ab-spider)

(defun simple-fetch (url)
  (multiple-value-bind (body status-code headers) (drakma:http-request url)
    (when (/= status-code 200)
      (error 'http-error :code status-code :headers headers))
    
    body))

(define-condition http-error (error)
  ((code :initarg :code :reader code)
   (headers :initarg :headers :reader headers))
  (:report (lambda (condition stream)
             (format stream
                     "HTTP Error: ~a~%~%Headers:~%~a"
                     (code condition)
                     (headers condition)))))

(defun crawl-url (url)
  (crawl-string (simple-fetch url) url))


(defun looks-like-an-url (string)
  (if (starts-with-subseq "http" string)
      string
      nil))

(defun crawl-string (html-string &optional (base-url ""))
  (let ((document (chtml:parse html-string (cxml-stp:make-builder)))
        (strings nil)
        (hrefs nil))
    (stp:do-recursively (element document)
      (when (typep element 'stp:element)
        (push (stp:string-value element) strings)
        (when (equal (stp:local-name element) "a")
          (let ((new-url (stp:attribute-value element "href")))
            (push (if (looks-like-an-url new-url)
                      new-url
                      (concatenate 'string base-url new-url))
                  hrefs)))))
    hrefs))

