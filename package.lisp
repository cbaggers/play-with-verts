;;;; package.lisp

(defpackage #:play-with-verts
  (:use #:cl #:cepl #:rtg-math #:nineveh #:vari
        #:cepl.skitter :temporal-functions
        :nineveh.vignette
        :nineveh.anti-aliasing
        :with-setf))
