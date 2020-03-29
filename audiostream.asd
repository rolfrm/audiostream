;;;; audiostream.asd

(asdf:defsystem #:audiostream
  :description "Describe audiostream here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-pulseaudio-simple)
  :components ((:file "package")
               (:file "audiostream")))
