;;; Clojure PSM (Problem Space Management) wrapper for test compatibility
;;; This provides the same interface as techne-psm.lisp for the test files

(ns techne-psm
  (:require [techne-atms :refer :all :exclude [update]]))

(defn create-rekb
  "Create a reasoning engine knowledge base (ATMS)"
  [name]
  (create-atms name {:debugging false :reporting true}))

(defn contradiction
  "Get the contradiction node from the ATMS"
  [rekb]
  (:contra-node @rekb))

(def ^:dynamic *labels* (atom {}))

(defn declare-atomic
  "Declare an atomic node with a label and sort (:GOAL or :TASK)"
  [atom label sort rekb]
  (if (or (= sort :DA) (= sort :TASK))
    ;; For tasks and defeasible assumptions, create an assumption-based node
    (let [ass-node (tms-create-node rekb label {:assumption? false})
          fake-label (str "fake-" label)
          fakenode (tms-create-node rekb fake-label {:assumption? true})]
      (justify-node (str "fake-just-" label) ass-node [fakenode])
      (swap! *labels* assoc fake-label fakenode)
      ass-node)
    ;; For goals, create a simple node
    (tms-create-node rekb label {:assumption? false})))

(defn assrt-formula
  "Assert a formula with consequent, antecedents, sort, label, and rekb"
  [consequent ants sort label rekb]
  (when (= sort :DA)
    (let [fake-label (str "fakeform-" label)
          ass-node (tms-create-node rekb fake-label {:assumption? true})]
      (swap! *labels* assoc fake-label ass-node)
      (let [new-ants (cons ass-node ants)]
        (justify-node label consequent new-ants)))))

(defn print-summary
  "Print a summary of the REKB/ATMS statistics"
  [rekb]
  (print-atms-summary rekb))
