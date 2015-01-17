(in-package #:bdf-parser)

(defmacro define-typed-struct (name &body members)
  (labels ((slot-definition (spec)
             (destructuring-bind (name type) spec
               `(,name :type     ,type
                       :initarg  ,(intern (symbol-name name) "KEYWORD")
                       :accessor ,name))))
    `(defclass ,name ()
       ,(mapcar #'slot-definition members))))

(deftype direction ()
  '(member :horizontal :vertical :both))

(deftype maybe (type)
  `(or ,type null))

(define-typed-struct offset
  (x-offset integer)
  (y-offset integer))

(define-typed-struct bounding-box
  (width  integer)
  (height integer)
  (offset offset))

(define-typed-struct property
  (name  string)
  (value string))

(define-typed-struct metrics
  (bounding-box           bounding-box)
  (scalable-step          (maybe offset))
  (device-step            (maybe offset))
  (scalable-step-vertical (maybe offset))
  (device-step-vertical   (maybe offset))
  (vvector                (maybe offset)))

(define-typed-struct glyph
  (name              string)
  (encoding          integer)
  (standard-encoding boolean)
  (metrics           metrics)
  (bitmap            (array bit (* *))))

(define-typed-struct font
  (bdf-version     string)
  (content-version (maybe string))
  (name            string)
  (comments        list)
  (properties      list)
  (point-size      integer)
  (x-resolution    integer)
  (y-resolution    integer)
  (direction       direction)
  (metrics         metrics)
  (glyphs          (vector glyph)))

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
