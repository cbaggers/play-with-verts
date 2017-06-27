;;;; play-with-verts.asd

(asdf:defsystem #:play-with-verts
  :description "Describe play-with-verts here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cepl.sdl2 #:nineveh #:dirt #:cepl.issac
                           #:temporal-functions)
  :components ((:file "package")
               (:file "play-with-verts")))
