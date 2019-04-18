(uiop/package:define-package :gh-dist/dist (:use :cl :ql-dist) (:export :setup :install-gh) (:nicknames :gh-dist))
(in-package :gh-dist/dist)
;;;don't edit above

(defclass gh-dist (dist) ())

(defmethod install-metadata-file ((dist gh-dist))
  (relative-to dist "gh-info.txt"))

(defun gh-dist-enumeration-function ()
  "The default function used for producing a list of dist objects."
  (loop for file in (directory (ql:qmerge "dists/*/gh-info.txt"))
        collect (ql-dist::make-dist-from-file file :class 'gh-dist)))

(defmethod available-versions ((dist gh-dist))
  ;;tbd.... need it?
  )

(defun filter-gh-release (&key allow-draft allow-prerelease)
  (lambda (release)
    (and (if allow-draft
             t
             (not (getf release :|draft|)))
         (if allow-prerelease
             t
             (not (getf release :|prerelease|)))
         (let ((list (mapcar (lambda (x) (getf x :|name|)) (getf release :|assets|))))
           (and (find "releases.txt" list :test 'equal)
                (find "systems.txt" list :test 'equal)))
         (getf release :|tag_name|))))

(defun fetch-dist (user/repos file &key version)
  (unless version
    (let* ((url (format nil "https://api.github.com/repos/~A/releases" user/repos))
           (cont (jojo:parse (dex:get url))))
      (setf version
            (getf (first (remove-if-not (filter-gh-release) cont)) :|tag_name|))))
  (with-open-file (s file
                       :direction :output
                       :if-exists :rename-and-delete)
      (format s "~{~{~A: ~A~%~}~}"
              `(("name" ,(substitute #\- #\/ user/repos))
                ("version" ,version)
                ("system-index-url" ,(format nil "https://github.com/~A/releases/download/~A/systems.txt"
                                             user/repos version))
                ("release-index-url" ,(format nil "https://github.com/~A/releases/download/~A/releases.txt"
                                              user/repos version))))))

(defun install-gh (url &key (prompt t) replace version)
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
  (let ((url (substitute #\/ #\-  (name dist)))
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
