(uiop/package:define-package :gh-dist/https (:use :cl) (:import-from :dexador) (:export :fetch-via-dexador))
(in-package :gh-dist/https)
;;;don't edit above

(defun fetch-via-dexador (url file &key (follow-redirects t) quietly (maximum-redirects 10))
  "Request URL and write the body of the response to FILE."
  (declare (ignorable follow-redirects quietly maximum-redirects))
  (dex:fetch (ql-http::urlstring (ql-http:url url)) file
             :if-exists :supersede)
  (values (make-instance 'ql-http::header :status 200)
          (probe-file file)))
