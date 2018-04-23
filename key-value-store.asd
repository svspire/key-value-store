;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "key-value-store"
  :author "Shannon Spires <ssos@bearlanding.com>"
  :version "0.1.1"
  :maintainer "Shannon Spires <ssos@bearlanding.com>"
  :licence "BSD 3-clause"
  :description "Generic Key/Value API for Common Lisp."
  :serial t
  :components ((:file "package")
               (:file "key-value-store")))
        
