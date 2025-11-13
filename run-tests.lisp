;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

(load "techne-atms.lisp")

(load "fiveam/fiveam.lisp")

(load "techne-atms-test.lisp")

(fiveam:with-dribble (:text)
  (fiveam:run! :techne-atms))
