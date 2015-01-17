(in-package #:bdf-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+keywords+)
    (defconstant +keywords+
      '(STARTFONT COMMENT CONTENTVERSION FONT SIZE FONTBOUNDINGBOX METRICSSET
        SWIDTH DWIDTH SWIDTH1 DWIDTH1 VVECTOR STARTPROPERTIES ENDPROPERTIES
        CHARS STARTCHAR ENCODING BBX BITMAP ENDCHAR ENDFONT))))


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
    (make-instance 'font
      :bdf-version     (cdr bdf-version)
      :content-version (getprop  'contentversion body)
      :name            (getprop  'font           body)
      :comments        (getprop* 'comment        body)
      :properties      (getprop  'properties     body)
      :point-size      point-size
      :x-resolution    xres
      :y-resolution    yres
      :direction       (ecase (getprop 'metricsset body)
                         ((0 nil) :horizontal)
                         ((1)     :vertical)
                         ((2)     :both))
      :metrics         (make-instance 'metrics
                         :bounding-box           (getprop 'fontboundingbox body)
                         :scalable-step          (getprop 'swidth  body)
                         :device-step            (getprop 'dwidth  body)
                         :scalable-step-vertical (getprop 'swidth1 body)
                         :device-step-vertical   (getprop 'dwidth1 body)
                         :vvector                (getprop 'vvector body))
      :glyphs          (coerce (getprop 'chars body) 'vector))))

(defun parse-properties (startproperties properties endproperties)
  (declare (ignore endproperties))
  (let ((n (cdr startproperties)))
    (when (/= n (length properties))
      (warn "Unexpected number of properties"))
    (cons 'properties properties)))

(defun parse-chars (startchar chars)
  (let ((n (cdr startchar)))
    (when (/= n (length chars))
      (warn "Unexpected number of glyphs"))
    (cons 'chars chars)))

(defun round-up (n m)
  (* m (ceiling n m)))

(defun parse-startchar (name body endchar)
  (declare (ignore endchar))
  (destructuring-bind (encoding1 &optional encoding2) (getprop 'encoding body)
    (let ((bbx (getprop 'bbx body)))
      (make-instance 'glyph
        :name              (cdr name)
        :encoding          (or encoding2 encoding1)
        :standard-encoding (null encoding2)
        :metrics           (make-instance 'metrics
                             :bounding-box           bbx
                             :scalable-step          (getprop 'swidth  body)
                             :device-step            (getprop 'dwidth  body)
                             :scalable-step-vertical (getprop 'swidth1 body)
                             :device-step-vertical   (getprop 'dwidth1 body)
                             :vvector                (getprop 'vvector body))
        :bitmap            (let* ((bb-width  (width bbx))
                                  (bb-height (height bbx))
                                  (data      (getprop 'bitmap body))
                                  (iwidth    (round-up bb-width 8)))
                             (loop
                                with bitmap = (make-array (list bb-height bb-width) :element-type 'bit)
                                for row from 0 below bb-height
                                for raw-num in data
                                do (loop
                                      with num = (ash raw-num (- bb-width iwidth))
                                      for col from (1- bb-width) downto 0
                                      do (setf (aref bitmap row col) (logand num 1))
                                      do (setf num (ash num -1)))
                                finally (return bitmap)))))))

(defun parse-bitmap (bitmap-line data)
  (declare (ignore bitmap-line))
  (cons 'bitmap data))

(defun parse-hex (data newline)
  (declare (ignore newline))
  (parse-integer data :radix 16))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro delay (f)
    (let ((args (gensym "-ARGS")))
      `(function (lambda (&rest ,args)
                   (apply ,f ,args)))))

  (defun parse-line* (constructor &rest constructors)
    (lambda (keyword &rest args)
      (cons keyword (apply constructor (mapcar #'funcall constructors (butlast args))))))

  (defun parse-line (&rest constructors)
    (apply #'parse-line* #'identity constructors)))

(define-parser *bdf-parser*
  (:start-symbol bdf)
  (:terminals #.(list* :newline :string +keywords+))

  (bdf               (startfont-line       global-lines     endfont-line        (delay #'parse-bdf)))
  (properties-block  (startproperties-line property-lines   endproperties-line  (delay #'parse-properties)))
  (chars-block       (chars-line           startchar-blocks                     (delay #'parse-chars)))
  (startchar-block   (startchar-line       charprop-lines   endchar-line        (delay #'parse-startchar)))
  (bitmap-block      (bitmap-line          hex-lines                            (delay #'parse-bitmap)))

  ;;                      KEYWORD         ARG1    ARG2    ARG3    ARG4                           CONSTRUCTOR          PARSER1         PARSER2         PARSER3         PARSER4
  (startfont-line        (STARTFONT       :string                         :newline  (parse-line                       #'identity)))
  (endfont-line          (ENDFONT                                         :newline))
  (comment-line          (COMMENT         :string                         :newline  (parse-line                       #'identity)))
  (contentversion-line   (CONTENTVERSION  :string                         :newline  (parse-line                       #'identity)))
  (font-line             (FONT            :string                         :newline  (parse-line                       #'identity)))
  (size-line             (SIZE            :string :string :string         :newline  (parse-line* #'list               #'parse-integer #'parse-integer #'parse-integer)))
  (fontboundingbox-line  (FONTBOUNDINGBOX :string :string :string :string :newline  (parse-line* #'make-bounding-box* #'parse-integer #'parse-integer #'parse-integer #'parse-integer)))
  (metricsset-line       (METRICSSET      :string                         :newline  (parse-line                       #'parse-integer)))
  (swidth-line           (SWIDTH          :string :string                 :newline  (parse-line* #'make-offset        #'parse-integer #'parse-integer)))
  (dwidth-line           (DWIDTH          :string :string                 :newline  (parse-line* #'make-offset        #'parse-integer #'parse-integer)))
  (swidth1-line          (SWIDTH1         :string :string                 :newline  (parse-line* #'make-offset        #'parse-integer #'parse-integer)))
  (dwidth1-line          (DWIDTH1         :string :string                 :newline  (parse-line* #'make-offset        #'parse-integer #'parse-integer)))
  (vvector-line          (VVECTOR         :string :string                 :newline  (parse-line* #'make-offset        #'parse-integer #'parse-integer)))
  (startproperties-line  (STARTPROPERTIES :string                         :newline  (parse-line                       #'parse-integer)))
  (property-line         (:string         :string                         :newline  (parse-line                       #'identity)))
  (endproperties-line    (ENDPROPERTIES                                   :newline))
  (chars-line            (CHARS           :string                         :newline  (parse-line                       #'parse-integer)))
  (startchar-line        (STARTCHAR       :string                         :newline  (parse-line                       #'identity)))
  (endchar-line          (ENDCHAR                                         :newline))
  (encoding-line         (ENCODING        :string                         :newline  (parse-line* #'list               #'parse-integer))
                         (ENCODING        :string :string                 :newline  (parse-line* #'list               #'parse-integer #'parse-integer)))
  (bbx-line              (BBX             :string :string :string :string :newline  (parse-line* #'make-bounding-box* #'parse-integer #'parse-integer #'parse-integer #'parse-integer)))
  (bitmap-line           (BITMAP                                          :newline))

  (hex-line              (:string                                         :newline  (delay #'parse-hex)))

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
