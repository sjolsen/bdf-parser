(asdf:defsystem #:bdf-parser
  :description "Parser for Glyph BDF files"
  :depends-on (#:yacc)
  :components ((:file "bdf-parser")))
