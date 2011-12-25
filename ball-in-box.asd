(asdf:defsystem #:ball-in-box
  :version "0.0.0"
  :depends-on (#:log5
               #:lispbuilder-sdl
               #:lispbuilder-sdl-mixer
               #:lispbuilder-sdl-ttf
               #:cl-opengl
               ;; Somehow, if these aren't the last deps iolib or clsql fails to build :(
               #:alexandria
               #:uffi
               #:cffi)
  :components ((:module "src" :components
                        ((:file "package")
                         (:file "logging" :depends-on ("package"))
                         (:file "ball-in-box" :depends-on ("package" "logging"))))))
