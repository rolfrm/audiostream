;;;; package.lisp

(defpackage #:audiostream
  (:use #:cl #:pulseaudio-simple)
  (:export :audio-start :audio-stop :audio-add-gen)
  )
