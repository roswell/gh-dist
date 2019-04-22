(uiop/package:define-package :gh-dist/dist
  (:use :cl :ql-dist)
  (:shadow :install)
  (:export :setup :install)
  (:nicknames :gh-dist))
(in-package :gh-dist/dist)
;;;don't edit above

(defclass gh-dist (dist) ())

(defmethod ql-dist::distinfo-subscription-url ((dist gh-dist))
  (format nil "github://~A/"
          (substitute #\/ #\- (name dist) :count 1)))

(defmethod install-metadata-file ((dist gh-dist))
  (relative-to dist "gh-info.txt"))

(defun gh-dist-enumeration-function ()
  "The default function used for producing a list of dist objects."
  (loop for file in (directory (ql:qmerge "dists/*/gh-info.txt"))
        collect (ql-dist::make-dist-from-file file :class 'gh-dist)))

(defmethod available-versions ((dist gh-dist))
  (let ((url (format nil "https://api.github.com/repos/~A/git/refs/tags"
                     (substitute #\/ #\- (name dist) :count 1))))
    (flet ((ref (x)
             (search "\"ref\":" x)))
      (nreverse
       (mapcar (lambda (x)
                 (let ((version (format nil "~{~A~^/~}"
                                        (butlast (cdddr (uiop:split-string
                                                         (second (uiop:split-string x :separator '(#\:)))
                                                         :separator'(#\" #\/)))))))
                   (cons version (format nil "~A~A"
                                         (ql-dist::distinfo-subscription-url dist)
                                         version))))
               (apply #'append
                      (mapcar (lambda (x)
                                (remove-if-not #'ref (uiop:split-string x :separator '(#\,))))
                              (remove-if-not #'ref (uiop:split-string (dex:get url) :separator '(#\{))))))))))

(defun fetch-dist (user/repos file &key version)
  (unless version
    (setf version
          (first (available-versions (make-instance 'gh-dist :name user/repos)))))
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

(defun install (url &key (prompt t) replace version)
  (block nil
    (let ((temp-file (ql:qmerge "tmp/install-dist-distinfo.txt")))
      (ensure-directories-exist temp-file)
      (ql-util:delete-file-if-exists temp-file)
      (fetch-dist url temp-file :version version)
      (let* ((new-dist (ql-dist::make-dist-from-file temp-file :class 'gh-dist))
             (old-dist (find-dist (name new-dist))))
        (when old-dist
          (if replace
              (uninstall old-dist)
              (restart-case
                  (error "A dist named ~S is already installed."
                         (name new-dist))
                (replace ()
                  :report "Replace installed dist with new dist"
                  (uninstall old-dist)))))
        (format t "Installing dist ~S version ~S.~%"
                (name new-dist)
                (version new-dist))
        (when (or (not prompt)
                  (ql-util:press-enter-to-continue))
          (ensure-directories-exist (base-directory new-dist))
          (ql-util:copy-file temp-file (relative-to new-dist "gh-info.txt"))
          (ql-dist::ensure-release-index-file new-dist)
          (ql-dist::ensure-system-index-file new-dist)
          (enable new-dist)
          (setf (preference new-dist) (get-universal-time))
          (when old-dist
            (ql-dist::clear-dist-systems old-dist))
          (ql-dist::clear-dist-systems new-dist)
          new-dist)))))

(defmethod available-update ((dist gh-dist))
  (let ((url (substitute #\/ #\- (name dist) :count 1))
        (target (ql:qmerge "tmp/distinfo-update/gh-info.txt"))
        (update-directory (ql:qmerge "tmp/distinfo-update/")))
    (when (ql-dist::probe-directory update-directory)
      (ql-dist::delete-directory-tree (ql:qmerge "tmp/distinfo-update/")))
    (when url
      (ensure-directories-exist target)
      (fetch-dist url target)
      (let ((new (ql-dist::make-dist-from-file target :class 'gh-dist)))
        (setf (base-directory new)
              (make-pathname :name nil
                             :type nil
                             :version nil
                             :defaults target))
        (when (and (string= (name dist) (name new))
                   (string/= (version dist) (version new)))
          new)))))

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
