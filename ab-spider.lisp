;;;; ab-spider.lisp
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:ab-spider)

(defun http-p (string)
  (if (starts-with-subseq "http" string)
      string
      nil))

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

(defclass spider ()
  ()
  (:documentation "A default spider that doesn't do anything."))

(defgeneric initialize (spider)
  (:documentation "Perform any initialization that the spider may need."))
(defgeneric reset (spider)
  (:documentation "Reset the spider and delete may need."))
  
(defgeneric crawl (spider url)
  (:documentation "Crawl the site at URL and add to the spider's database."))

(defgeneric add-keyword (spider url keyword)
  (:documentation "Process a keyword"))

(defgeneric add-link (spider src dst)
  (:documentation "Process a link from src to dst. "))

(defgeneric add-url (spider full-url)
  (:documentation "Process an URL."))

(defgeneric classify-url (spider full-url)
  (:documentation "Classify full-url as new, already scanned, invalid, etc. "))

(defclass sqlite-spider (spider)
  ((db-url :initform "/Users/jeremiahlarocco/data/search_data/abs.db" :initarg :db-url)))

(defmethod initialize ((spider sqlite-spider))
  (sqlite:with-open-database (db (slot-value spider 'db-url))
    (sqlite:execute-non-query db "create table hosts (id integer primary key, name text unique not null)")
    (sqlite:execute-non-query db "create table paths (id integer primary key, path text not null)")

    (sqlite:execute-non-query db "create table urls (id integer primary key, secure integer not null default 0, host integer not null, path integer not null, crawled integer default 0)")

    (sqlite:execute-non-query db "create table robot_filters (id integer primary key, host integer not null, pattern text not null)")

    (sqlite:execute-non-query db "create table keywords (id integer primary key, keyword text unique not null)")

    (sqlite:execute-non-query db "create table links (id integer primary key, src_url integer not null, dst_url integer not null)")
    (sqlite:execute-non-query db "create table keyword_mapping (id integer primary key, url integer not null, keyword integer not null)")

    ))

(defmethod reset ((spider sqlite-spider))
  (sqlite:with-open-database (db (slot-value spider 'db-url))
    (sqlite:execute-non-query db "drop table if exists hosts")
    (sqlite:execute-non-query db "drop table if exists paths")
    (sqlite:execute-non-query db "drop table if exists urls")

    (sqlite:execute-non-query db "drop table if exists robot_filters")
    
    (sqlite:execute-non-query db "drop table if exists keywords")
    (sqlite:execute-non-query db "drop table if exists links")
    (sqlite:execute-non-query db "drop table if exists keyword_mapping")
    ))



(defmethod crawl ((spider sqlite-spider) url)
  (sqlite:with-open-database (db (slot-value spider 'db-url))
    (let ((uri (puri:uri url)))
      (declare (ignorable uri))
      
      ;; (with-slots (visited-urls robot-files) spider
      ;;   (multiple-value-bind (ignore found) (gethash url visited-urls)
      ;;     (declare (ignorable ignore))
      ;;     (when (not found)
      ;;       (setf (gethash url visited-urls) t)
      ;;       (when-let ((robpots-info (gethash (puri:uri-host uri) robot-files)))
      ;;         (let ((document (chtml:parse html-string (cxml-stp:make-builder)))
      ;;               (strings nil)
      ;;               (hrefs nil))
      ;;           (stp:do-recursively (element document)
      ;;             (when (typep element 'stp:element)
      ;;               (push (stp:string-value element) strings)
      ;;               (when (equal (stp:local-name element) "a")
      ;;                 (let ((new-url (stp:attribute-value element "href")))
      ;;                   (push (if (looks-like-an-url new-url)
      ;;                             new-url
      ;;                             (concatenate 'string base-url new-url))
      ;;                         hrefs))))))))))
      (values))))

(defmethod add-keyword ((spider sqlite-spider) url keyword)
  (sqlite:with-open-database (db (slot-value spider 'db-url))))

(defmethod add-link ((spider sqlite-spider) src dst)
  (sqlite:with-open-database (db (slot-value spider 'db-url))))

(defmethod add-url ((spider sqlite-spider) full-url)
  (sqlite:with-open-database (db (slot-value spider 'db-url))))

