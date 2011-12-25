(defpackage #:ball-in-box
  (:use :cl)
  (:use :log5)
  (:export :start-logging
           :ball-in-box))

(in-package :ball-in-box)
(arnesi:enable-sharp-l)

(defvar *root* (asdf:system-source-directory :afdog)
  "What is considered the root of the application.")

(defvar *window* `(:width 1024 :height 768
                   :title "Ball-In-Box")
  "A bundle of window parameters")
