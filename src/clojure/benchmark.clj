(ns benchmark
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [update]))

(load-file "techne_atms.clj")

(require '[techne-atms :refer :all])

;;; Benchmark utilities

(defn time-ms
  "Execute function f and return execution time in milliseconds"
  [f]
  (let [start (System/nanoTime)
        result (f)
        end (System/nanoTime)]
    {:result result
     :time-ms (/ (- end start) 1000000.0)}))

(defn mean [numbers]
  (/ (reduce + numbers) (count numbers)))

(defn std-dev [numbers]
  (let [avg (mean numbers)
        squared-diffs (map #(Math/pow (- % avg) 2) numbers)]
    (Math/sqrt (/ (reduce + squared-diffs) (count numbers)))))

;;; Synthetic benchmark generators

(defn create-chain-problem
  "Create a chain of nodes A1 -> A2 -> ... -> An
   Tests justification propagation"
  [atms n]
  (let [nodes (atom [])]
    ;; Create assumptions
    (dotimes [i n]
      (swap! nodes conj (tms-create-node atms (str "A" i) {:assumption? true})))
    ;; Create chain of justifications
    (dotimes [i (dec n)]
      (let [result (tms-create-node atms (str "N" i))]
        (justify-node (str "J" i) result [(@nodes i) (@nodes (inc i))])
        (swap! nodes conj result)))
    @nodes))

(defn create-pyramid-problem
  "Create a pyramid structure where each level depends on previous level
   Tests weave function heavily. Each level has one fewer node than previous."
  [atms levels width]
  (let [all-nodes (atom [])
        level-starts (atom [0])]  ;; Track where each level starts
    ;; Level 0: assumptions
    (dotimes [i width]
      (swap! all-nodes conj (tms-create-node atms (str "L0-" i) {:assumption? true})))
    (swap! level-starts conj (count @all-nodes))

    ;; Subsequent levels - each level has one fewer node
    (dotimes [level (dec levels)]
      (let [prev-start (@level-starts level)
            prev-end (@level-starts (inc level))
            prev-count (- prev-end prev-start)
            prev-nodes (subvec @all-nodes prev-start prev-end)
            level-num (inc level)
            new-count (dec prev-count)]  ;; One fewer node than previous
        (when (> new-count 0)
          (dotimes [i new-count]
            (let [result (tms-create-node atms (str "L" level-num "-" i))]
              ;; Each node depends on two adjacent nodes from previous level
              (justify-node (str "J" level-num "-" i)
                           result
                           [(prev-nodes i) (prev-nodes (inc i))])
              (swap! all-nodes conj result))))
        (swap! level-starts conj (count @all-nodes))))
    @all-nodes))

(defn create-contradictions-problem
  "Create a problem with multiple contradictions
   Tests nogood handling"
  [atms n-assumptions n-contradictions]
  (let [assumptions (atom [])]
    ;; Create assumptions
    (dotimes [i n-assumptions]
      (swap! assumptions conj (tms-create-node atms (str "A" i) {:assumption? true})))

    ;; Create contradictions
    (dotimes [i n-contradictions]
      ;; Each contradiction involves 2-3 random assumptions
      (let [n-nodes (+ 2 (rand-int 2))
            selected (take n-nodes (shuffle @assumptions))
            contra-node (tms-create-node atms (str "Contra" i))]
        (justify-node (str "C" i) contra-node selected)
        (justify-node (str "C" i "-false") (:contra-node @atms) [contra-node])))
    @assumptions))

(defn create-complex-problem
  "Create a complex problem combining multiple patterns"
  [atms n]
  (let [n-assumptions (int (* n 0.3))
        n-derived (int (* n 0.5))
        n-contradictions (int (* n 0.2))]

    ;; Create assumptions
    (let [assumptions (atom [])]
      (dotimes [i n-assumptions]
        (swap! assumptions conj (tms-create-node atms (str "A" i) {:assumption? true})))

      ;; Create derived nodes with multiple justifications
      (dotimes [i n-derived]
        (let [result (tms-create-node atms (str "D" i))
              n-ants (+ 2 (rand-int 3))
              ants (take n-ants (shuffle @assumptions))]
          (justify-node (str "J" i) result ants)))

      ;; Create contradictions
      (dotimes [i n-contradictions]
        (let [n-nodes (+ 2 (rand-int 2))
              selected (take n-nodes (shuffle @assumptions))
              contra-node (tms-create-node atms (str "Contra" i))]
          (justify-node (str "C" i) contra-node selected)
          (justify-node (str "C" i "-false") (:contra-node @atms) [contra-node])))

      @assumptions)))

;;; Benchmark definitions

(def benchmarks
  [{:name "chain-25"
    :description "Chain of 25 nodes"
    :fn (fn [atms] (create-chain-problem atms 25))}

   {:name "chain-50"
    :description "Chain of 50 nodes"
    :fn (fn [atms] (create-chain-problem atms 50))}

   {:name "chain-75"
    :description "Chain of 75 nodes"
    :fn (fn [atms] (create-chain-problem atms 75))}

   {:name "pyramid-5x10"
    :description "Pyramid 5 levels x 10 width"
    :fn (fn [atms] (create-pyramid-problem atms 5 10))}

   {:name "pyramid-7x8"
    :description "Pyramid 7 levels x 8 width"
    :fn (fn [atms] (create-pyramid-problem atms 7 8))}

   {:name "contradictions-30-10"
    :description "30 assumptions with 10 contradictions"
    :fn (fn [atms] (create-contradictions-problem atms 30 10))}

   {:name "contradictions-50-20"
    :description "50 assumptions with 20 contradictions"
    :fn (fn [atms] (create-contradictions-problem atms 50 20))}

   {:name "complex-50"
    :description "Complex problem with 50 nodes"
    :fn (fn [atms] (create-complex-problem atms 50))}

   {:name "complex-100"
    :description "Complex problem with 100 nodes"
    :fn (fn [atms] (create-complex-problem atms 100))}

   {:name "complex-150"
    :description "Complex problem with 150 nodes (max safe size)"
    :fn (fn [atms] (create-complex-problem atms 150))}

   ;; Larger benchmarks for testing parallel performance
   {:name "chain-100"
    :description "Chain of 100 nodes"
    :fn (fn [atms] (create-chain-problem atms 100))}

   {:name "chain-150"
    :description "Chain of 150 nodes"
    :fn (fn [atms] (create-chain-problem atms 150))}

   {:name "chain-200"
    :description "Chain of 200 nodes (large)"
    :fn (fn [atms] (create-chain-problem atms 200))}

   {:name "pyramid-10x15"
    :description "Pyramid 10 levels x 15 width (large)"
    :fn (fn [atms] (create-pyramid-problem atms 10 15))}

   {:name "contradictions-100-30"
    :description "100 assumptions with 30 contradictions (large)"
    :fn (fn [atms] (create-contradictions-problem atms 100 30))}

   {:name "complex-200"
    :description "Complex problem with 200 nodes (large)"
    :fn (fn [atms] (create-complex-problem atms 200))}

   {:name "complex-300"
    :description "Complex problem with 300 nodes (very large)"
    :fn (fn [atms] (create-complex-problem atms 300))}])

;;; Benchmark runner

(defn run-benchmark
  "Run a single benchmark with given parallel setting"
  [benchmark parallel? num-cores]
  (let [atms (create-atms (str "Benchmark-" (:name benchmark))
                         {:parallel? parallel?
                          :num-cores num-cores
                          :parallel-threshold 30
                          :debugging false
                          :reporting false})
        result (time-ms #((:fn benchmark) atms))]
    {:benchmark (:name benchmark)
     :description (:description benchmark)
     :parallel? parallel?
     :num-cores num-cores
     :time-ms (:time-ms result)
     :nodes (count (:nodes @atms))
     :environments (count (:env-table @atms))
     :nogoods (count (:nogood-table @atms))}))

(defn run-benchmark-suite
  "Run all benchmarks with both parallel and sequential, N times each"
  [n-runs num-cores]
  (println (format "\n=== Running Benchmark Suite ==="))
  (println (format "Runs per configuration: %d" n-runs))
  (println (format "Available cores: %d" num-cores))
  (println (format "Total benchmarks: %d" (count benchmarks)))
  (println (format "Total runs: %d\n" (* (count benchmarks) 2 n-runs)))

  (let [results (atom [])]
    (doseq [bench benchmarks]
      (println (format "Running: %s - %s" (:name bench) (:description bench)))

      ;; Sequential runs
      (print "  Sequential: ")
      (dotimes [run n-runs]
        (print (format "%d " (inc run)))
        (flush)
        (let [result (run-benchmark bench false 1)]
          (swap! results conj (assoc result :run (inc run) :config "sequential"))))
      (println "✓")

      ;; Parallel runs
      (print (format "  Parallel(%d): " num-cores))
      (dotimes [run n-runs]
        (print (format "%d " (inc run)))
        (flush)
        (let [result (run-benchmark bench true num-cores)]
          (swap! results conj (assoc result :run (inc run) :config "parallel"))))
      (println "✓"))

    @results))

;;; CSV output

(defn results-to-csv
  "Convert benchmark results to CSV format"
  [results]
  (let [header "benchmark,description,config,run,parallel,num_cores,time_ms,nodes,environments,nogoods"
        rows (map (fn [r]
                   (str/join ","
                            [(:benchmark r)
                             (str "\"" (:description r) "\"")
                             (:config r)
                             (:run r)
                             (:parallel? r)
                             (:num-cores r)
                             (:time-ms r)
                             (:nodes r)
                             (:environments r)
                             (:nogoods r)]))
                 results)]
    (str/join "\n" (cons header rows))))

(defn save-results
  "Save benchmark results to CSV file"
  [results filename]
  (spit filename (results-to-csv results))
  (println (format "\nResults saved to: %s" filename)))

;;; Analysis

(defn analyze-results
  "Compute statistics from benchmark results"
  [results]
  (println "\n=== Benchmark Analysis ===\n")

  (doseq [bench-name (distinct (map :benchmark results))]
    (let [bench-results (filter #(= (:benchmark %) bench-name) results)
          seq-times (map :time-ms (filter #(= "sequential" (:config %)) bench-results))
          par-times (map :time-ms (filter #(= "parallel" (:config %)) bench-results))
          seq-mean (mean seq-times)
          par-mean (mean par-times)
          speedup (/ seq-mean par-mean)
          seq-std (std-dev seq-times)
          par-std (std-dev par-times)]

      (println (format "%s:" bench-name))
      (println (format "  Sequential: %.2f ms (±%.2f)" seq-mean seq-std))
      (println (format "  Parallel:   %.2f ms (±%.2f)" par-mean par-std))
      (println (format "  Speedup:    %.2fx" speedup))
      (println))))

;;; Main execution

(defn run-performance-analysis
  "Main entry point for performance analysis"
  [& {:keys [runs cores output-file]
      :or {runs 3
           cores (detect-cores)
           output-file "benchmark_results.csv"}}]

  (println "\n" (str/join "" (repeat 60 "=")))
  (println "  ATMS Performance Benchmark Suite")
  (println (str/join "" (repeat 60 "=")))

  (let [results (run-benchmark-suite runs cores)]
    (analyze-results results)
    (save-results results output-file)

    (println "\n" (str/join "" (repeat 60 "=")))
    (println "  Benchmark Complete!")
    (println (str/join "" (repeat 60 "=")))
    (println (format "\nGenerate graphs with: python3 plot_benchmarks.py %s" output-file))

    results))

;; Enable running from command line
(defn -main [& args]
  (run-performance-analysis))
