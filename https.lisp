(uiop/package:define-package :gh-dist/https (:use :cl) (:export :register-fetch))
(in-package :gh-dist/https)
;;;don't edit above

(defun fetch-via-dexador (url file &key (follow-redirects t) quietly (maximum-redirects 10))
  "Request URL and write the body of the response to FILE."
  (declare (ignorable follow-redirects quietly maximum-redirects))
  (dex:fetch (ql-http::urlstring (ql-http:url url)) file
             :if-exists :supersede)
  (values (make-instance 'ql-http::header :status 200)
          (probe-file file)))

(defun register-fetch (&key overwirte (methods '("https")))
  (declare (ignorable overwirte methods))
  (dolist (x methods)
    (when (or (not (find x ql-http:*fetch-scheme-functions* :test 'equal :key 'first))
              overwirte)
      (setf ql-http:*fetch-scheme-functions*
            (acons x 'fetch-via-dexador
                   (remove x ql-http:*fetch-scheme-functions* :key 'first :test 'equal))))))
