;;don't edit
(defsystem "gh-dist"
  :depends-on(:dexador
              :jonathan)
  :class :package-inferred-system
  :components((:file "https")
              (:file "dist"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com")
