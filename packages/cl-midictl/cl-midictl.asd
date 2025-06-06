;;;; cl-midictl.asd

(asdf:defsystem #:cl-midictl
  :description "Abstraction layer handling the state and connection of
  hardware midi-controllers to incudine."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "Gnu Public license, version 2.0 or later."
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-refs #:orm-utils #:incudine)
  :components ((:file "package")
               (:file "globals")
               (:file "structs")
               (:file "midi-port")
               (:file "osc-midi")
               (:file "utils")
               (:file "cl-midictl")
               (:file "nanoktl2")
               (:file "nanoktl2-preset-midi")
               (:file "faderfox")
               (:file "asparion")
               ))
