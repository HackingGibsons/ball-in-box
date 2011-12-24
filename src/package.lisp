(defpackage #:ball-in-box
  (:use :cl)
  (:use :log5)
  (:export :start-logging
           :ball-in-box))

(in-package :ball-in-box)

(defvar *root* (asdf:system-source-directory :afdog)
  "What is considered the root of the application.")

(defvar *fps* 60
  "The frame-rate we use")

(defvar *window* `(:width 1024 :height 768
                   :title "Ball-In-Box")
  "A bundle of window parameters")
