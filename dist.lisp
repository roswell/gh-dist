(uiop/package:define-package :gh-dist/dist (:use :cl :ql-dist) (:export :setup) (:nicknames :gh-dist))
(in-package :gh-dist/dist)
;;;don't edit above

(defclass gh-dist (dist) ())

(defmethod install-metadata-file ((dist gh-dist))
  (relative-to dist "gh-info.txt"))

(defun gh-dist-enumeration-function ()
  "The default function used for producing a list of dist objects."
  (loop for file in (directory (ql:qmerge "dists/*/gh-info.txt"))
        collect (ql-dist::make-dist-from-file file :class 'gh-dist)))

(defun account (str/uri)
  (let* ((sep (uiop:split-string str/uri :separator '(#\/)))
         (pos (position "github.com" sep :test #'equal)))
    (if pos
        (format nil "~{~A~^/~}" (subseq sep (1+ pos) (+ 3 pos)))
        str/uri)))

(defmethod available-versions ((dist gh-dist))
  ;;tbd
  )

;;(dex:get "https://api.github.com/repos/roswell/quicklisp/releases")
;;(gethash "link" (nth-value 2 (dex:get "https://api.github.com/repos/roswell/quicklisp/releases")))
;;(gethash "link" (nth-value 2 (dex:get "https://api.github.com/repositories/74199560/releases?page=4")))

;;; dist-update
(defun install-github (url &key (prompt t) replace)
  ;;tbd
  )

(defun setup (&key (enable t))
  (if enable 
      (progn 
        (pushnew 'gh-dist-enumeration-function *dist-enumeration-functions*)
        (gh-dist/https:register-fetch))
      (setf *dist-enumeration-functions*
            (remove 'gh-dist-enumeration-function *dist-enumeration-functions*)
            ;; can't undo https support.
            ))
  t)

