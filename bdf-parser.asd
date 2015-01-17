(asdf:defsystem #:bdf-parser
  :description "Parser for Glyph BDF files"
  :depends-on (#:yacc)
  :components ((:file "package")
               (:file "types"
                      :depends-on ("package"))
               (:file "parser"
                      :depends-on ("package"
                                   "types"))))
