(asdf:defsystem #:bdf-parser
  :description "Parser for Glyph BDF files"
  :depends-on (#:yacc)
  :components ((:file "package")
               (:file "bdf-types"
                      :depends-on ("package"))
               (:file "bdf-parser"
                      :depends-on ("package"
                                   "bdf-types"))))
