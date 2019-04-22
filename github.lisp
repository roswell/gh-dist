(uiop/package:define-package :gh-dist/github
  (:use :cl :ql-dist)
  (:export :gh-dist :fetch-gh-dist))
(in-package :gh-dist/github)
;;;don't edit above
(defclass gh-dist (dist) ())

(defmethod ql-dist::distinfo-subscription-url ((dist gh-dist))
  (format nil "github://~A/"
          (substitute #\/ #\- (name dist) :count 1)))

(defmethod install-metadata-file ((dist gh-dist))
  (relative-to dist "gh-info.txt"))

(defmethod available-versions ((dist gh-dist))
  (let ((temp (ql:qmerge "tmp/dist-versions.txt"))
        (url (format nil "https://api.github.com/repos/~A/git/refs/tags"
                     (substitute #\/ #\- (name dist) :count 1))))
    (ensure-directories-exist temp)
    (ql-util:delete-file-if-exists temp)
    (handler-case
        (ql-http:fetch url temp :quietly t)
      (ql-http:unexpected-http-status ()
        (return-from available-versions nil)))
    (flet ((ref (x)
             (search "\"ref\":" x)))
      (nreverse
       (mapcar (lambda (x)
                 (let ((version (format nil "~{~A~^/~}"
                                        (butlast (cdddr (uiop:split-string
                                                         (second (uiop:split-string x :separator '(#\:)))
                                                         :separator'(#\" #\/)))))))
                   (cons version (format nil "~A~A" (ql-dist::distinfo-subscription-url dist) version))))
               (apply #'append
                      (mapcar (lambda (x)
                                (remove-if-not #'ref (uiop:split-string x :separator '(#\,))))
                              (remove-if-not #'ref (uiop:split-string (uiop:read-file-string temp) :separator '(#\{))))))))))

(defmethod available-update ((dist gh-dist))
  (let ((url (substitute #\/ #\- (name dist) :count 1))
        (target (ql:qmerge "tmp/distinfo-update/gh-info.txt"))
        (update-directory (ql:qmerge "tmp/distinfo-update/")))
    (when (ql-dist::probe-directory update-directory)
      (ql-dist::delete-directory-tree (ql:qmerge "tmp/distinfo-update/")))
    (when url
      (ensure-directories-exist target)
      (fetch-gh-dist url target)
      (let ((new (ql-dist::make-dist-from-file target :class 'gh-dist)))
        (setf (base-directory new)
              (make-pathname :name nil
                             :type nil
                             :version nil
                             :defaults target))
        (when (and (string= (name dist) (name new))
                   (string/= (version dist) (version new)))
          new)))))

(defun fetch-gh-dist (user/repos file &key version)
  (unless version
    (setf version
          (first (first (available-versions (make-instance 'gh-dist :name user/repos))))))
  (with-open-file (s file
                     :direction :output
                     :if-exists :rename-and-delete)
    (format s "~{~{~A: ~A~%~}~}"
            `(("name" ,(substitute #\- #\/ user/repos :count 1))
              ("version" ,version)
              ("system-index-url" ,(format nil "https://github.com/~A/releases/download/~A/systems.txt"
                                           user/repos version))
              ("release-index-url" ,(format nil "https://github.com/~A/releases/download/~A/releases.txt"
                                            user/repos version))))))
