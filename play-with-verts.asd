;;;; play-with-verts.asd

(asdf:defsystem #:play-with-verts
  :description "Describe play-with-verts here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:cepl.sdl2
               #:nineveh
               :dirt
               :cepl.skitter.sdl2
               :temporal-functions
               :rtg-math
               :rtg-math.vari
               :sdl2-game-controller-db
               :with-setf)
  :components ((:file "package")
               (:file "utils")
               (:file "render")
               (:file "assets")
               (:file "camera")
               (:file "things")
               (:file "blur")
               (:file "fxaa")
               (:file "play-with-verts")))
