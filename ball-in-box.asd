(asdf:defsystem #:ball-in-box
  :version "0.0.0"
  :depends-on (#:log5
               #:arnesi
               #:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:cl-opengl)
  :components ((:module "src" :components
                        (;; Basics
                         (:file "package")
                         (:file "logging" :depends-on ("package"))

                         ;; Internals
                         (:file "game-world" :depends-on ("package" "logging"))
                         (:file "objects" :depends-on ("game-world"))

                         ;; Drivers
                         (:file "ball-in-box" :depends-on ("game-world" "objects"))))))
