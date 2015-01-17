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

;;; TODO: Accept strings with internal quotes
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

(defun getprop (key body)
  (cdr (assoc key body)))

(defun getprop* (key body)
  (loop
     for cell in body
     when (eq key (car cell))
       collect (cdr cell)))

(defun parse-bdf (bdf-version body endfont)
  (declare (ignore endfont))
  (destructuring-bind (point-size xres yres) (getprop 'size body)
    (destructuring-bind (bb-width bb-height bb-off) (getprop 'fontboundingbox body)
      (make-instance 'font
        :bdf-version     bdf-version
        :content-version (getprop  'content-version body)
        :name            (getprop  'font            body)
        :comments        (getprop* 'comment         body)
        :properties      (getprop  'properties      body)
        :point-size      point-size
        :x-resolution    xres
        :y-resolution    yres
        :direction       (ecase (getprop 'metricsset body)
                           ((0 nil) :horizontal)
                           ((1)     :vertical)
                           ((2)     :both))
        :metrics         (make-instance 'metrics
                           :bounding-box    (make-bounding-box bb-width bb-height bb-off)
                           :scalable-width  (getprop 'swidth  body)
                           :scalable-height (getprop 'swidth1 body)
                           :device-width    (getprop 'dwidth  body)
                           :device-height   (getprop 'dwidth1 body)
                           :vvector         (getprop 'vvector body))
        :glyphs          (getprop  'chars body)))))

(defun parse-properties (n properties endproperties)
  (declare (ignore endproperties))
  (when (/= n (length properties))
    (warn "Unexpected number of properties"))
  properties)

(defun parse-chars (n chars)
  (when (/= n (length chars))
    (warn "Unexpected number of glyphs"))
  chars)

(defun round-up (n m)
  (* m (ceiling n m)))

(defun parse-startchar (name body endchar)
  (declare (ignore endchar))
  (destructuring-bind (encoding standardp) (getprop 'encoding body)
    (let ((bbx (getprop 'bbx body)))
      (make-instance 'glyph
        :name              name
        :encoding          encoding
        :standard-encoding standardp
        :metrics           (make-instance 'metrics
                             :bounding-box    bbx
                             :scalable-width  (getprop 'swidth  body)
                             :scalable-height (getprop 'swidth1 body)
                             :device-width    (getprop 'dwidth  body)
                             :device-height   (getprop 'dwidth1 body)
                             :vvector         (getprop 'vvector body))
        :bitmap            (let* ((width  (first bbx))
                                  (height (second bbx))
                                  (data   (getprop 'bitmap body))
                                  (iwidth (round-up width 8)))
                             (map `(vector (bit-vector ,width) ,height)
                                  (lambda (num)
                                    (setf num (ash num (- width iwidth)))
                                    (loop
                                       with v = (make-array width :element-type 'bit)
                                       for i from (1- width) downto 0
                                       do (setf (aref v i) (logand num 1))
                                       do (setf num (ash num -1))
                                       finally (return v)))
                                  data))))))

(defun parse-hex (data newline)
  (declare (ignore newline))
  (parse-integer data :radix 16))

(defun parse-line (&rest constructors)
  (lambda (keyword &rest args)
    (cons keyword (mapcar #'funcall constructors (butlast args)))))

(define-parser *bdf-parser*
  (:start-symbol bdf)
  (:terminals #.(list* :newline :string +keywords+))

  (bdf               (startfont-line       global-lines     endfont-line        #'parse-bdf))
  (properties-block  (startproperties-line property-lines   endproperties-line  #'parse-properties))
  (chars-block       (chars-line           startchar-blocks                     #'parse-chars))
  (startchar-block   (startchar-line       charprop-lines   endchar-line        #'parse-startchar))
  (bitmap-block      (bitmap-line          hex-lines                            #'second))

  (startfont-line        (STARTFONT       :string                         :newline  (parse-line #'identity)))
  (endfont-line          (ENDFONT                                         :newline))
  (comment-line          (COMMENT         :string                         :newline  (parse-line #'identity)))
  (contentversion-line   (CONTENTVERSION  :string                         :newline  (parse-line #'identity)))
  (font-line             (FONT            :string                         :newline  (parse-line #'identity)))
  (size-line             (SIZE            :string :string :string         :newline  (parse-line #'parse-integer #'parse-integer #'parse-integer)))
  (fontboundingbox-line  (FONTBOUNDINGBOX :string :string :string :string :newline  (parse-line #'parse-integer #'parse-integer #'parse-integer #'parse-integer)))
  (metricsset-line       (METRICSSET      :string                         :newline  (parse-line #'parse-integer)))
  (swidth-line           (SWIDTH          :string :string                 :newline  (parse-line #'parse-integer #'parse-integer)))
  (dwidth-line           (DWIDTH          :string :string                 :newline  (parse-line #'parse-integer #'parse-integer)))
  (swidth1-line          (SWIDTH1         :string :string                 :newline  (parse-line #'parse-integer #'parse-integer)))
  (dwidth1-line          (DWIDTH1         :string :string                 :newline  (parse-line #'parse-integer #'parse-integer)))
  (vvector-line          (VVECTOR         :string :string                 :newline  (parse-line #'parse-integer #'parse-integer)))
  (startproperties-line  (STARTPROPERTIES :string                         :newline  (parse-line #'parse-integer)))
  (property-line         (:string         :string                         :newline  (parse-line #'identity)))
  (endproperties-line    (ENDPROPERTIES                                   :newline))
  (chars-line            (CHARS           :string                         :newline  (parse-line #'parse-integer)))
  (startchar-line        (STARTCHAR       :string                         :newline  (parse-line #'identity)))
  (endchar-line          (ENDCHAR                                         :newline))
  (encoding-line         (ENCODING        :string                         :newline  (parse-line #'parse-integer))
                         (ENCODING        :string :string                 :newline  (parse-line #'parse-integer #'parse-integer)))
  (bbx-line              (BBX             :string :string :string :string :newline  (parse-line #'parse-integer #'parse-integer #'parse-integer #'parse-integer)))
  (bitmap-line           (BITMAP                                          :newline))
  (hex-line              (:string                                         :newline  #'parse-hex))

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

(defun parse (stream)
  (parse-with-lexer (bdf-lexer stream) *bdf-parser*))
