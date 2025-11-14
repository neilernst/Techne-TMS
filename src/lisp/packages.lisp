;;;; -*- Mode: LISP; Syntax: Common-lisp; -*-
;;;; Package definitions for Techne-TMS

(defpackage #:techne-atms
  (:use #:common-lisp)
  (:export
   ;; Main ATMS structures
   #:atms
   #:tms-node
   #:just
   #:env

   ;; ATMS creation and management
   #:create-atms
   #:change-atms
   #:atms-title
   #:atms-node-counter
   #:atms-just-counter
   #:atms-env-counter
   #:atms-nodes
   #:atms-justs
   #:atms-contradictions
   #:atms-assumptions
   #:atms-debugging
   #:atms-nogood-table
   #:atms-contra-node
   #:atms-env-table
   #:atms-empty-env
   #:atms-node-string
   #:atms-enqueue-procedure

   ;; Node operations
   #:tms-create-node
   #:assume-node
   #:make-contradiction
   #:justify-node
   #:nogood-nodes
   #:remove-node
   #:true-node?
   #:in-node?
   #:out-node?
   #:node-consistent-with?

   ;; Node accessors
   #:tms-node-index
   #:tms-node-datum
   #:tms-node-label
   #:tms-node-justs
   #:tms-node-consequences
   #:tms-node-contradictory?
   #:tms-node-assumption?
   #:tms-node-rules
   #:tms-node-atms

   ;; Justification accessors
   #:just-index
   #:just-informant
   #:just-consequence
   #:just-antecedents

   ;; Environment operations
   #:create-env
   #:union-env
   #:cons-env
   #:find-or-make-env
   #:lookup-env
   #:subset-env?
   #:compare-env
   #:env?
   #:env-nogood?

   ;; Environment accessors
   #:env-index
   #:env-count
   #:env-assumptions
   #:env-nodes
   #:env-rules

   ;; Explanation
   #:explain-node

   ;; Printing and debugging
   #:node-string
   #:why-node
   #:why-nodes
   #:node-justifications
   #:print-justification
   #:print-env
   #:env-string
   #:print-nogoods
   #:print-envs
   #:print-atms-statistics))

(defpackage #:techne-psm
  (:use #:common-lisp #:techne-atms)
  (:export
   ;; Distance functions
   #:min-effort
   #:max-fam
   #:simple))
