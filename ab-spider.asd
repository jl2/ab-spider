;;;; ab-spider.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:ab-spider
  :description "Describe ab-spider here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:alexandria
               #:drakma
               #:cl-who
               #:st-json
               #:sqlite
               #:closure-html
               #:cxml
               #:cxml-stp)
  :serial t
  :components ((:file "package")
               (:file "ab-spider")))

