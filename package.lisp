;;;; package.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage #:ab-spider
  (:use #:cl #:alexandria)
  (:export #:spider
           #:sqlite-spider

           #:initialize
           #:reset

           #:crawl))



