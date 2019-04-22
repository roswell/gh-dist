(uiop/package:define-package :gh-dist/dist
  (:use :cl :ql-dist)
  (:import-from :gh-dist/github :gh-dist :fetch-gh-dist)
  (:shadow :install)
  (:export :setup :install)
  (:nicknames :gh-dist))
(in-package :gh-dist/dist)
;;;don't edit above

(defun gh-dist-enumeration-function ()
  "The default function used for producing a list of dist objects."
  (loop for file in (directory (ql:qmerge "dists/*/gh-info.txt"))
        collect (ql-dist::make-dist-from-file file :class 'gh-dist)))

(defun install (url &key (prompt t) replace version)
  (block nil
    (let ((temp-file (ql:qmerge "tmp/install-dist-distinfo.txt")))
      (ensure-directories-exist temp-file)
      (ql-util:delete-file-if-exists temp-file)
      (fetch-gh-dist url temp-file :version version)
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

(defun register-fetch (&key overwrite
                            (methods '())
                            (function nil))
  (declare (ignorable methods))
  (unless function
    (error "function should be set for register-fetch"))
  (dolist (x methods)
    (when (or (not (find x ql-http:*fetch-scheme-functions* :test 'equal :key 'first))
              overwrite)
      (setf ql-http:*fetch-scheme-functions*
            (acons x function
                   (remove x ql-http:*fetch-scheme-functions* :key 'first :test 'equal))))))

(defun setup (&key (enable t))
  (if enable 
      (progn
        (pushnew 'gh-dist-enumeration-function *dist-enumeration-functions*)
        (unless (assoc "https" ql-http:*fetch-scheme-functions* :test 'equal)
          (ql:quickload :gh-dist/https)
          (uiop:symbol-call :gh-dist/https :register-fetch
                            :methods '("https")
                            :function (find-symbol (string :fetch-via-dexador)
                                                   :gh-dist/https))))
      (setf *dist-enumeration-functions*
            (remove 'gh-dist-enumeration-function *dist-enumeration-functions*)))
  t)
