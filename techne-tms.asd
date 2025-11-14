;;;; -*- Mode: LISP; Syntax: Common-lisp; -*-
;;;; ASDF system definition for Techne-TMS

(defsystem "techne-tms"
  :description "Techne Truth Maintenance System - ATMS implementation"
  :version "1.0.0"
  :author "Neil Ernst"
  :license "GPL-3.0"
  :components ((:file "packages")
               (:file "techne-atms" :depends-on ("packages"))))

(defsystem "techne-tms/psm"
  :description "Problem Solver Module for Techne-TMS"
  :depends-on (:techne-tms :cl-graph)
  :components ((:file "techne-psm")
               (:file "psm-graph" :depends-on ("techne-psm"))))

(defsystem "techne-tms/tests"
  :description "Test suite for Techne-TMS"
  :depends-on (:techne-tms :fiveam)
  :components ((:file "techne-atms-test"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :techne-atms)))
