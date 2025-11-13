(ns techne-atms-test
  (:require [clojure.test :refer :all]
            [techne-atms :refer :all])
  (:refer-clojure :exclude [update]))

(deftest test-atms-creation
  (let [atms (create-atms "Test ATMS")]
    (is (instance? clojure.lang.Atom atms))
    (is (= "Test ATMS" (:title @atms)))))

(deftest test-node-creation
  (let [atms (create-atms "Test ATMS")]
    (let [node (tms-create-node atms "Test Node")]
      (is (instance? clojure.lang.Atom node))
      (is (= "Test Node" (:datum @node))))))

(deftest test-assumption-creation
  (let [atms (create-atms "Test ATMS")]
    (let [node (tms-create-node atms "Test Assumption" {:assumption? true})]
      (is (:assumption? @node))
      (is (seq (:label @node))))))

(deftest test-justification
  (let [atms (create-atms "Test ATMS")]
    (let [a1 (tms-create-node atms "A1" {:assumption? true})
          a2 (tms-create-node atms "A2" {:assumption? true})
          n1 (tms-create-node atms "N1")]
      (justify-node "Test Justification" n1 [a1 a2])
      (is (seq (:justs @n1)))
      (is (seq (:label @n1))))))
