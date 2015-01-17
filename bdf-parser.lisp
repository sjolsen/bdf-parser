(defpackage #:bdf-parser
  (:use #:cl #:yacc)
  (:export #:bdf-parse))

(in-package #:bdf-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+keywords+)
    (defconstant +keywords+
      '(STARTFONT COMMENT CONTENTVERSION FONT SIZE FONTBOUNDINGBOX METRICSSET
        SWIDTH DWIDTH SWIDTH1 DWIDTH1 VVECTOR STARTPROPERTIES ENDPROPERTIES
        CHARS STARTCHAR ENCODING BBX BITMAP ENDCHAR ENDFONT))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export +keywords+ '#:bdf-parser))


;;;; Lexing

(defun read-string (stream)
  (loop
     for c = (read-char stream nil :eof)
     when (char= c #\Newline)
       do (unread-char c stream)
     until (member c '(#\Space #\Newline :eof))
     collecting c into clist
     finally (return (coerce clist 'string))))

(defun lex-one (stream)
  (let ((c (read-char stream nil :eof)))
    (case c
      ((:eof)      :eof)
      ((#\Space)   (lex-one stream))
      ((#\Newline) :newline)
      ((#\")       (unread-char c stream)
                   (read-preserving-whitespace stream))
      (t           (unread-char c stream)
                   (read-string stream)))))

(defun classify (string)
  (if (member string +keywords+ :key #'symbol-name :test #'string=)
      (let ((symbol (intern string '#:bdf-parser)))
        (values symbol symbol))
      (values :string string)))

(defun bdf-lexer (stream)
  (lambda ()
    (let ((token (lex-one stream)))
      (case token
        ((:eof)     (values nil nil))
        ((:newline) (values :newline nil))
        (t          (classify token))))))


;;;; Parsing

(defun first* (thing1 &rest rest)
  (declare (ignore rest))
  thing1)

(defun *list (l1 &rest rest)
  (append l1 rest))

(defun *list-drop (l1 &rest rest)
  (append l1 (butlast rest)))

(defun droplast (&rest rest)
  (butlast rest))

(define-parser *bdf-parser*
  (:start-symbol bdf)
  (:terminals #.(list* :newline :string +keywords+))

  (bdf               (startfont-line       global-lines     endfont-line        #'*list-drop))
  (properties-block  (startproperties-line property-lines   endproperties-line  #'*list-drop))
  (chars-block       (chars-line           startchar-blocks                     #'*list))
  (startchar-block   (startchar-line       charprop-lines   endchar-line        #'*list-drop))
  (bitmap-block      (bitmap-line          hex-lines                            #'*list))

  (startfont-line        (STARTFONT       :string                         :newline  #'droplast))
  (endfont-line          (ENDFONT                                         :newline  #'droplast))
  (comment-line          (COMMENT         :string                         :newline  #'droplast))
  (contentversion-line   (CONTENTVERSION  :string                         :newline  #'droplast))
  (font-line             (FONT            :string                         :newline  #'droplast))
  (size-line             (SIZE            :string :string :string         :newline  #'droplast))
  (fontboundingbox-line  (FONTBOUNDINGBOX :string :string :string :string :newline  #'droplast))
  (metricsset-line       (METRICSSET      :string                         :newline  #'droplast))
  (swidth-line           (SWIDTH          :string :string                 :newline  #'droplast))
  (dwidth-line           (DWIDTH          :string :string                 :newline  #'droplast))
  (swidth1-line          (SWIDTH1         :string :string                 :newline  #'droplast))
  (dwidth1-line          (DWIDTH1         :string :string                 :newline  #'droplast))
  (vvector-line          (VVECTOR         :string :string                 :newline  #'droplast))
  (startproperties-line  (STARTPROPERTIES :string                         :newline  #'droplast))
  (property-line         (:string         :string                         :newline  #'droplast))
  (endproperties-line    (ENDPROPERTIES                                   :newline  #'droplast))
  (chars-line            (CHARS           :string                         :newline  #'droplast))
  (startchar-line        (STARTCHAR       :string                         :newline  #'droplast))
  (endchar-line          (ENDCHAR                                         :newline  #'droplast))
  (encoding-line         (ENCODING        :string                         :newline  #'droplast)
                         (ENCODING        :string :string                 :newline  #'droplast))
  (bbx-line              (BBX             :string :string :string :string :newline  #'droplast))
  (bitmap-line           (BITMAP                                          :newline  #'droplast))
  (hex-line              (:string                                         :newline  #'first*))

  (global-line  comment-line
                contentversion-line
                font-line
                size-line
                fontboundingbox-line
                metricsset-line
                swidth-line
                dwidth-line
                swidth1-line
                dwidth1-line
                vvector-line
                properties-block
                chars-block)

  (charprop-line  encoding-line
                  swidth-line
                  dwidth-line
                  swidth1-line
                  dwidth1-line
                  vvector-line
                  bbx-line
                  bitmap-block)

  (global-lines      (global-line     global-lines      #'list*)  ())
  (property-lines    (property-line   property-lines    #'list*)  ())
  (startchar-blocks  (startchar-block startchar-blocks  #'list*)  ())
  (charprop-lines    (charprop-line   charprop-lines    #'list*)  ())
  (hex-lines         (hex-line        hex-lines         #'list*)  ()))

(defun bdf-parse (stream)
  (parse-with-lexer (bdf-lexer stream) *bdf-parser*))
