;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

(load "techne-atms.lisp")

(defpackage #:techne-atms-test
  (:use #:common-lisp #:fiveam))

(in-package #:techne-atms-test)

;;; Test suite definition
(def-suite :techne-atms)
(in-suite :techne-atms)

(test test-atms-creation
  (let ((atms (create-atms "Test ATMS")))
    (is (typep atms 'atms))
    (is (string= "Test ATMS" (atms-title atms)))))

(test test-node-creation
  (let ((atms (create-atms "Test ATMS"))
        (node (tms-create-node atms "Test Node")))
    (is (typep node 'tms-node))
    (is (string= "Test Node" (tms-node-datum node)))))

(test test-assumption-creation
  (let* ((atms (create-atms "Test ATMS"))
         (node (tms-create-node atms "Test Assumption" :assumptionp t)))
    (is (tms-node-assumption? node))
    (is (not (null (tms-node-label node))))))

(test test-justification
  (let* ((atms (create-atms "Test ATMS"))
         (a1 (tms-create-node atms "A1" :assumptionp t))
         (a2 (tms-create-node atms "A2" :assumptionp t))
         (n1 (tms-create-node atms "N1")))
    (justify-node "Test Justification" n1 (list a1 a2))
    (is (not (null (tms-node-justs n1))))
    (is (not (null (tms-node-label n1))))))

(test test-contradiction
  (let* ((atms (create-atms "Test ATMS"))
         (a1 (tms-create-node atms "A1" :assumptionp t))
         (n1 (tms-create-node atms "N1")))
    (justify-node "Test Justification" n1 (list a1))
    (make-contradiction n1)
    (is (env-nogood? (first (tms-node-label a1))))))
