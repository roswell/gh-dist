;;don't edit
(defsystem "gh-dist"
  :depends-on(:dexador)
  :class :package-inferred-system
  :components(#+quicklisp(:file "https")
              #+quicklisp(:file "dist"))
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com")
