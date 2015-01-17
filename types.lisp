(in-package #:bdf-parser)

(deftype direction ()
  '(member :horizontal :vertical :both))

(deftype maybe (type)
  `(or ,type null))

(defclass offset ()
  ((x-offset :type     integer
             :initarg  :x-offset
             :accessor x-offset)
   (y-offset :type     integer
             :initarg  :y-offset
             :accessor y-offset)))

(defclass bounding-box ()
  ((width  :type     integer
           :initarg  :width
           :accessor width)
   (height :type     integer
           :initarg  :height
           :accessor height)
   (offset :type     offset
           :initarg  :offset
           :accessor offset)))

(defclass property ()
  ((name  :type     string
          :initarg  :name
          :accessor name)
   (value :type     string
          :initarg  :value
          :accessor value)))

(defclass metrics ()
  ((bounding-box :type     bounding-box
                 :initarg  :bounding-box
                 :accessor bounding-box)

   (scalable-step          :type (maybe offset)
                           :initarg :scalable-step
                           :accessor scalable-step)
   (device-step            :type (maybe offset)
                           :initarg :device-step
                           :accessor device-step)
   (scalable-step-vertical :type (maybe offset)
                           :initarg :scalable-step-vertical
                           :accessor scalable-step-vertical)
   (device-step-vertical   :type (maybe offset)
                           :initarg :device-step-vertical
                           :accessor device-step-vertical)

   (vvector :type     (maybe offset)
            :initarg  :vvector
            :accessor vvector)))

(defclass glyph ()
  ((name :type     string
         :initarg  :name
         :accessor name)

   (encoding          :type     integer
                      :initarg  :encoding
                      :accessor encoding)
   (standard-encoding :type     boolean
                      :initarg  :standard-encoding
                      :accessor standard-encoding)

   (metrics :type     metrics
            :initarg  :metrics
            :accessor metrics)

   (bitmap :type     (array bit (* *))
           :initarg  :bitmap
           :accessor bitmap)))

(defclass font ()
  ((bdf-version     :type     string
                    :initarg  :bdf-version
                    :accessor bdf-version)
   (content-version :type     (maybe string)
                    :initarg  :content-version
                    :accessor content-version)

   (name :type     string
         :initarg  :name
         :accessor name)

   (comments   :type     list
               :initarg  :comments
               :accessor comments)
   (properties :type     list
               :initarg  :properties
               :accessor properties)

   (point-size   :type     integer
                 :initarg  :point-size
                 :accessor point-size)
   (x-resolution :type     integer
                 :initarg  :x-resolution
                 :accessor x-resolution)
   (y-resoltuion :type     integer
                 :initarg  :y-resolution
                 :accessor y-resoltuion)

   (direction :type     direction
              :initarg  :direction
              :initform :horizontal
              :accessor direction)

   (metrics :type     metrics
            :initarg  :metrics
            :accessor metrics)

   (glyphs :type     (vector glyph)
           :initarg  :glyphs
           :accessor glyphs)))

(defun make-offset (x-offset y-offset)
  (make-instance 'offset
    :x-offset x-offset
    :y-offset y-offset))

(defun make-bounding-box (width height offset)
  (make-instance 'bounding-box
    :width  width
    :height height
    :offset offset))

(defun make-bounding-box* (width height x-offset y-offset)
  (make-instance 'bounding-box
    :width  width
    :height height
    :offset (make-offset x-offset y-offset)))

(defun make-property (name value)
  (make-instance 'property
    :name  name
    :value value))
