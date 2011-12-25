(asdf:defsystem #:ball-in-box
  :version "0.0.0"
  :depends-on (#:log5
               #:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:cl-opengl
               ;; Somehow, if these aren't the last deps iolib or clsql fails to build :(
               #:alexandria
               #:uffi
               #:cffi)
  :components ((:module "src" :components
                        (;; Basics
                         (:file "package")
                         (:file "logging" :depends-on ("package"))

                         ;; Internals
                         (:file "game-world" :depends-on ("package" "logging"))

                         ;; Drivers
                         (:file "ball-in-box" :depends-on ("game-world"))))))
