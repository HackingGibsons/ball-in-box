(asdf:defsystem #:ball-in-box
  :version "0.0.0"
  :depends-on (#:log5
               #:arnesi
               #:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:lispbuilder-sdl-ttf
               #:cl-opengl)
  :components ((:module "src" :components
                        (;; Basics
                         (:file "package")
                         (:file "logging" :depends-on ("package"))
                         (:file "math" :depends-on ("logging"))

                         ;; Internals
                         (:file "game-world" :depends-on ("package" "logging" "objects"))
                         (:file "objects" :depends-on ("math"))

                         ;; Drivers
                         (:file "ball-in-box" :depends-on ("game-world" "objects"))))))
