;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-

(defsystem "kvs-tests"
  :version "0.0.2"
  :description "Key-Value-Store Tests"
  :author "Copyright (c) 2018 Shannon Spires <ssos@bearlanding.com>"
  :license "BSD 3-clause"
  :depends-on (key-value-store lisp-unit)
  :perform (test-op (o s)
             (symbol-call :lisp-unit :run-tests
                          :all :kvs-test))
  :components ((:module package
                        :pathname "tests/"
                        :components ((:file "package")))
               (:module tests
                        :depends-on (package)
                        :pathname "tests/"
                        :components ((:file "kvs-test")))))
