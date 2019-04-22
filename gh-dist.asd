;;don't edit
(defsystem "gh-dist"
  :class :package-inferred-system
  :depends-on (#+quicklisp"gh-dist/dist")
  :author "SANO Masatoshi"
  :mailto "snmsts@gmail.com")
(defsystem "ql-dist" :depends-on ("quicklisp"))
