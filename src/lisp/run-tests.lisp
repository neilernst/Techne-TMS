;;; -*- Mode: LISP; Syntax: Common-lisp; -*-
;;;; Test runner for Techne-TMS using ASDF

;; Load Quicklisp (if available)
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Ensure FiveAM is available
(ql:quickload :fiveam)

;; Load the ASDF system definition
(asdf:load-asd (merge-pathnames "techne-tms.asd" *default-pathname-defaults*))

;; Load the test system
(asdf:load-system :techne-tms/tests)

;; Run the tests
(in-package #:techne-atms-test)

(fiveam:run! :techne-atms)
