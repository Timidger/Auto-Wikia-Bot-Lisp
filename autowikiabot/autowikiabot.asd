(in-package :cl-user)

(defpackage :autowikiabot-asd
  (:use :cl :asdf))

(in-package :autowikiabot-asd)

(defsystem :autowikiabot
  :serial t
  :components ((:file "drakma")
               (:file "cl-json")
	       (:file "yason")
               (:file "cl-reddit")
	       (:file "cl-ppcre"))
  ; Depends on nothing I guess?
  :depends-on (:drakma :cl-json :cl-yason :cl-reddit :cl-ppcre ))