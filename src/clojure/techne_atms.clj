;;; Clojure conversion of Techne-ATMS.lisp

(ns techne-atms
  (:refer-clojure :exclude [update]))

(declare
  tms-create-node create-env subset-env? union-env update new-nogood
  propagate weave update-label compare-env insert-in-table
  set-env-contradictory cons-env lookup-env remove-env-from-labels
  weave? explain-node-1 env-string print-justification
  print-env-table print-table)

(defrecord ATMS [title node-counter just-counter env-counter nodes justs contradictions assumptions debugging reporting nogood-table contra-node env-table empty-env node-string enqueue-procedure
                 ;; Parallel processing fields
                 parallel? num-cores parallel-threshold operation-thresholds])

(defrecord TMSNode [index datum label justs consequences contradictory? assumption? rules atms])

(defrecord Just [index informant consequence antecedents])

(defrecord Env [index count assumptions nodes nogood? rules])

(defmethod print-method ATMS [atms ^java.io.Writer w]
  (.write w (format "#<ATMS: %s>" (:title atms))))

(defmethod print-method TMSNode [node ^java.io.Writer w]
  (.write w (if (:assumption? node)
              (format "A-%d" (:index node))
              (format "#<NODE: %s>" ((:node-string @(:atms node)) node)))))

(defmethod print-method Just [just ^java.io.Writer w]
  (.write w (format "<%s %d>" (:informant just) (:index just))))

(defmethod print-method Env [env ^java.io.Writer w]
  (.write w (format "E-%d" (:index env))))

(defn node-string [node]
  ((:node-string @(:atms node)) node))

(defmacro debugging [atms msg & args]
  `(when (:debugging ~atms)
     (println (format ~msg ~@args))))

(defmacro reporting [atms msg & args]
  `(when (:reporting ~atms)
     (println (format ~msg ~@args))))

(defn default-node-string [n]
  (format "%s" (:datum @n)))

;;; Parallel processing utilities

(defn detect-cores []
  "Detect number of available CPU cores on this system.
   Returns the number of processors available to the JVM."
  (.availableProcessors (Runtime/getRuntime)))

(defn should-parallelize?
  "Determine if parallelization should be used for a collection.

   Args:
     atms - ATMS atom containing parallel configuration
     collection-size - Size of collection to be processed

   Returns:
     true if collection should be processed in parallel, false otherwise

   Parallelization is enabled when:
     1. :parallel? flag is true in ATMS
     2. Collection size >= :parallel-threshold
     3. More than 1 CPU core is available"
  [atms collection-size]
  (and (:parallel? @atms)
       (>= collection-size (:parallel-threshold @atms))
       (> (:num-cores @atms) 1)))

(defn parallel-threshold-for
  "Get the parallelization threshold for a specific operation.

   Args:
     atms - ATMS atom
     operation - Keyword identifying operation (:weave, :update-label, :new-nogood)

   Returns:
     Threshold value for this operation (falls back to :parallel-threshold if not specified)"
  [atms operation]
  (or (get-in @atms [:operation-thresholds operation])
      (:parallel-threshold @atms)
      10))  ;; Default fallback

(defn parallel-config-string
  "Generate a human-readable string describing parallel configuration.

   Args:
     atms - ATMS atom

   Returns:
     String describing parallel settings"
  [atms]
  (let [cfg @atms]
    (if (:parallel? cfg)
      (format "Parallel processing: ENABLED (cores=%d, threshold=%d)"
              (:num-cores cfg)
              (:parallel-threshold cfg))
      "Parallel processing: DISABLED")))

(defn ordered-insert [item coll test]
  (cond
    (empty? coll) (list item)
    (test item (first coll)) (cons item coll)
    (= item (first coll)) coll
    :else (cons (first coll) (ordered-insert item (rest coll) test))))

(defmacro ordered-push [item list test]
  `(swap! ~list ordered-insert ~item ~test))

(defn assumption-order [a1 a2]
  (< (:index @a1) (:index @a2)))

(defn env-order [e1 e2]
  (< (:index e1) (:index e2)))

;;; Basic inference engine interface.

(defn create-atms
  "Create a new ATMS instance.

   Args:
     title - Name/description of this ATMS

   Optional keyword args:
     :debugging - Enable debug output (default: false)
     :reporting - Enable progress reporting (default: false)
     :parallel? - Enable parallel processing (default: true)
     :num-cores - Number of cores to use (default: auto-detect)
     :parallel-threshold - Min collection size for parallelization (default: 10)
     :node-string - Custom node string formatter
     :enqueue-procedure - Custom enqueue procedure"
  ([title] (create-atms title {}))
  ([title {:keys [node-string debugging reporting enqueue-procedure
                  parallel? num-cores parallel-threshold operation-thresholds]
           :or {node-string default-node-string
                debugging false
                reporting false
                enqueue-procedure nil
                parallel? true
                num-cores nil
                parallel-threshold 10
                operation-thresholds {}}}]
   (let [detected-cores (or num-cores (detect-cores))
         atms (atom (->ATMS title 0 0 0 [] [] [] [] debugging reporting nil nil nil nil node-string enqueue-procedure
                            parallel? detected-cores parallel-threshold operation-thresholds))]
     (let [contra-node (tms-create-node atms "The contradiction" {:contradictory? true})
           empty-env (create-env atms [])]
       (swap! atms #(assoc % :contra-node contra-node :empty-env empty-env))
       (when reporting
         (println (format "Created ATMS: %s" title))
         (println (parallel-config-string atms)))
       atms))))

(defn change-atms [atms & {:keys [node-string debugging enqueue-procedure]}]
  (swap! atms #(-> %
                   (cond-> node-string (assoc :node-string node-string))
                   (cond-> debugging (assoc :debugging debugging))
                   (cond-> enqueue-procedure (assoc :enqueue-procedure enqueue-procedure)))))

(defn true-node? [node]
  (let [atms @(:atms @node)]
    (= (first (:label @node)) (:empty-env atms))))

(defn in-node?
  ([n] (not (empty? (:label @n))))
  ([n env]
   (some #(subset-env? % env) (:label @n))))

(defn out-node? [n env]
  (not (in-node? @n env)))

(defn node-consistent-with? [n env]
  (some #(not (:nogood? @(union-env % env)))
        (:label n)))

(defn tms-create-node
  ([atms datum] (tms-create-node atms datum {}))
  ([atms datum {:keys [assumption? contradictory?]
                :or {assumption? false contradictory? false}}]
   (let [new-index (inc (:node-counter @atms))
         node (atom (->TMSNode new-index datum nil [] [] contradictory? assumption? [] atms))]
     (swap! atms (fn [a] (-> a
                            (clojure.core/update :node-counter inc)
                            (clojure.core/update :nodes conj node))))
     (when contradictory?
       (swap! atms #(clojure.core/update % :contradictions conj node)))
     (when assumption?
       (swap! atms #(clojure.core/update % :assumptions conj node))
       (swap! node #(assoc % :label (list (create-env atms [node])))))
     ;; Report progress every 10 nodes
     (when (and (:reporting @atms) (zero? (mod new-index 10)))
       (println (format "  Created %d nodes..." new-index)))
     node)))

(defn assume-node [node]
  (when-not (:assumption? @node)
    (let [atms (:atms @node)]
      (debugging @atms "Converting %s into an assumption" (node-string @node))
      (swap! node #(assoc % :assumption? true))
      (swap! atms #(clojure.core/update % :assumptions conj node))
      (update (list (create-env atms [node]))
              @node
              nil))))

(defn make-contradiction [node]
  (let [atms (:atms @node)]
    (when-not (:contradictory? @node)
      (swap! node #(assoc % :contradictory? true))
      (swap! atms #(clojure.core/update % :contradictions conj node))
      (loop [nogood (first (:label @node))]
        (when nogood
          (new-nogood atms nogood nil)
          (recur (first (:label @node))))))))

(defn justify-node [informant consequence antecedents]
  (let [atms (:atms @consequence)
        new-index (inc (:just-counter @atms))
        just (atom (->Just new-index
                           informant
                           consequence
                           antecedents))]
    (swap! atms (fn [a] (-> a
                           (clojure.core/update :just-counter inc)
                           (clojure.core/update :justs conj just))))
    (swap! consequence #(clojure.core/update % :justs conj just))
    (doseq [node antecedents]
      (swap! node #(clojure.core/update % :consequences conj just)))
    (debugging @atms "Justifying %s in terms of %s on %s"
               (node-string @consequence)
               informant
               (map #(node-string @%) antecedents))
    ;; Report progress every 20 justifications
    (when (and (:reporting @atms) (zero? (mod new-index 20)))
      (println (format "  Created %d justifications..." new-index)))
    (propagate just nil (list (:empty-env @atms)))
    just))

(defn nogood-nodes [informant nodes]
  (let [atms (:atms @(first nodes))]
    (justify-node informant
                  (:contra-node @atms)
                  nodes)))

;;; Label updating

(defn propagate [just antecedent envs]
  (when-let [new-envs (weave antecedent envs (:antecedents @just))]
    (update new-envs (:consequence @just) @just)))

(defn update [new-envs consequence just]
  (let [atms (:atms @consequence)]
    (if (:contradictory? @consequence)
      (doseq [env new-envs]
        (new-nogood atms env just))
      (let [new-envs (update-label consequence new-envs)]
        (when (seq new-envs)
          (when-let [enqueuef (:enqueue-procedure @atms)]
            (doseq [rule (:rules @consequence)]
              (enqueuef rule))
            (swap! consequence #(assoc % :rules [])))
          (doseq [supported-just (:consequences @consequence)]
            (propagate supported-just consequence new-envs))
          (filter #(some #{%} (:label @consequence)) new-envs))))))

(defn update-label [node new-envs]
  (let [current-envs (atom (:label @node))]
    (doseq [new-env new-envs]
      (let [subsumed? (atom false)]
        (doseq [current-env @current-envs]
          (when-not @subsumed?
            (when-let [comparison (compare-env new-env current-env)]
              (case comparison
                :EQ (reset! subsumed? true)
                :S21 (reset! subsumed? true)
                :S12 (swap! current-envs (fn [envs] (remove #(= % current-env) envs)))))))
        (when-not @subsumed?
          (swap! current-envs conj new-env))))
    (let [final-new-envs (remove nil? (distinct @current-envs))]
      (doseq [new-env final-new-envs]
        (swap! new-env #(clojure.core/update % :nodes conj node)))
      (swap! node #(assoc % :label final-new-envs))
      final-new-envs)))

(defn weave [antecedent envs antecedents]
  (loop [remaining-antecedents (remove #{antecedent} antecedents)
         current-envs envs]
    (if (empty? remaining-antecedents)
      current-envs
      (let [node (first remaining-antecedents)
            next-envs (atom [])]
        (doseq [env current-envs]
          (doseq [node-env (:label @node)]
            (let [new-env (union-env env node-env)]
              (when-not (:nogood? @new-env)
                (let [subsumed? (atom false)]
                  (doseq [existing-env @next-envs]
                    (when-not @subsumed?
                      (when-let [comparison (compare-env new-env existing-env)]
                        (case comparison
                          :EQ (reset! subsumed? true)
                          :S21 (reset! subsumed? true)
                          :S12 (swap! next-envs #(remove #{existing-env} %))))))
                  (when-not @subsumed?
                    (swap! next-envs conj new-env)))))))
        (if (seq @next-envs)
          (recur (rest remaining-antecedents) @next-envs)
          [])))))

(defn in-antecedent? [nodes]
  (or (empty? nodes)
      (weave? (:empty-env @(:atms @(first nodes))) nodes)))

(defn weave? [env nodes]
  (if (empty? nodes)
    true
    (some (fn [e]
            (let [new-env (union-env e env)]
              (when-not (:nogood? @new-env)
                (weave? new-env (rest nodes)))))
          (:label @(first nodes)))))

(defn supporting-antecedent? [nodes env]
  (every? #(in-node? @% env) nodes))

(defn remove-node [node]
  (when (seq (:consequences @node))
    (throw (Exception. "Can't remove node with consequences")))
  (let [atms (:atms @node)]
    (swap! atms #(-> %
                     (clojure.core/update :nodes (fn [nodes] (remove #{node} nodes)))
                     (clojure.core/update :assumptions (fn [assumptions] (remove #{node} assumptions)))))
    (doseq [just (:justs @node)]
      (doseq [ant (:antecedents @just)]
        (swap! ant #(clojure.core/update % :consequences (fn [consequences] (remove #{just} consequences))))))
    (doseq [env (:label @node)]
      (swap! env #(clojure.core/update % :nodes (fn [nodes] (remove #{node} nodes)))))))

;;; Creating and extending environments.

(defn create-env [atms assumptions]
  (let [e (atom (->Env (inc (:env-counter @atms))
                      (count assumptions)
                      assumptions
                      []
                      nil
                      []))]
    (swap! atms #(-> %
                     (clojure.core/update :env-counter inc)
                     (clojure.core/update :env-table (fn [table] (insert-in-table table e)))))
    (set-env-contradictory atms e)
    e))

(defn union-env [e1 e2]
  (let [[smaller larger] (if (< (:count @e1) (:count @e2))
                           [e1 e2]
                           [e2 e1])]
    (reduce (fn [current-env assumption]
              (if (:nogood? @current-env)
                (reduced current-env)
                (cons-env assumption current-env)))
            larger
            (:assumptions @smaller))))

(defn cons-env [assumption env]
  (let [nassumes (ordered-insert assumption (:assumptions @env) assumption-order)]
    (or (lookup-env nassumes)
        (create-env (:atms @assumption) nassumes))))

(defn find-or-make-env [assumptions atms]
  (if (empty? assumptions)
    (:empty-env @atms)
    (or (lookup-env assumptions)
        (create-env atms assumptions))))

;;; Env tables.

(defn insert-in-table [table env]
  (let [count (:count @env)]
    (assoc table count (conj (get table count []) env))))

(defn lookup-env [assumes]
  (let [atms (:atms @(first assumes))
        table (:env-table @atms)]
    (some (fn [env]
            (when (= (:assumptions @env) assumes)
              env))
          (get table (count assumes)))))

(defn subset-env? [e1 e2]
  (cond
    (= e1 e2) true
    (> (:count @e1) (:count @e2)) false
    :else (every? (set (:assumptions @e2)) (:assumptions @e1))))

(defn compare-env [e1 e2]
  (cond
    (= e1 e2) :EQ
    (< (:count @e1) (:count @e2))
    (when (subset-env? e1 e2) :S12)
    (> (:count @e1) (:count @e2))
    (when (subset-env? e2 e1) :S21)))

;;; Processing nogoods

(defn new-nogood [atms cenv just]
  (debugging @atms "  %s new minimal nogood." (print-str @cenv))
  (when (:reporting @atms)
    (let [nogood-count (count (mapcat second (:nogood-table @atms)))]
      (when (zero? (mod (inc nogood-count) 5))
        (println (format "  Found %d nogoods..." (inc nogood-count))))))
  (swap! cenv #(assoc % :nogood? just))
  (remove-env-from-labels cenv atms)
  (swap! atms #(clojure.core/update % :nogood-table (fn [table] (insert-in-table table cenv))))
  (let [count (:count @cenv)]
    (doseq [[entry-count entries] (:nogood-table @atms)]
      (when (> entry-count count)
        (doseq [old entries]
          (when (subset-env? cenv old)
            (swap! atms #(clojure.core/update-in % [:nogood-table entry-count] (fn [olds] (remove #{old} olds))))))))
    (doseq [[entry-count entries] (:env-table @atms)]
      (when (> entry-count count)
        (doseq [old entries]
          (when (and (not (:nogood? @old))
                     (subset-env? cenv old))
            (swap! old #(assoc % :nogood? cenv))
            (remove-env-from-labels old atms)))))))

(defn set-env-contradictory [atms env]
  (when-not (:nogood? @env)
    (let [count (:count @env)]
      (loop [entries (:nogood-table @atms)]
        (when (seq entries)
          (let [[entry-count cenvs] (first entries)]
            (if (> entry-count count)
              false
              (if (some #(when (subset-env? % env)
                           (swap! env assoc :nogood? %)
                           true)
                        cenvs)
                true
                (recur (rest entries))))))))))

(defn remove-env-from-labels [env atms]
  (when-let [enqueuef (:enqueue-procedure @atms)]
    (doseq [rule (:rules @env)]
      (enqueuef rule))
    (swap! env #(assoc % :rules [])))
  (doseq [node (:nodes @env)]
    (swap! node #(clojure.core/update % :label (fn [label] (remove #{env} label))))))

;;; Generating explanations

(defn explain-node [node env]
  (explain-node-1 env node #{} []))

(defn explain-node-1 [env node queued-nodes explanation]
  (if (queued-nodes node)
    nil
    (if (and (:assumption? @node) (some #{node} (:assumptions @env)))
      (cons {:type :assume :node node} explanation)
      (if (some (fn [just]
                  (if (= (:type just) :assume)
                    (= (:node just) node)
                    (= (:consequence @(:just just)) node)))
                explanation)
        explanation
        (let [queued-nodes (conj queued-nodes node)]
          (loop [justs (:justs @node)
                 new-explanation explanation]
            (if (empty? justs)
              new-explanation
              (let [just (first justs)]
                (if (every? #(in-node? @% env) (:antecedents @just))
                  (let [final-explanation (reduce (fn [acc ant]
                                                    (if-let [exp (explain-node-1 env ant queued-nodes acc)]
                                                      exp
                                                      (reduced nil)))
                                                  (cons {:type :just :just just} new-explanation)
                                                  (:antecedents @just))]
                    (if final-explanation
                      (recur (rest justs) final-explanation)
                      (recur (rest justs) new-explanation)))
                  (recur (rest justs) new-explanation))))))))))

;;; Printing

(defn why-node
  ([node] (why-node node *out* ""))
  ([node stream] (why-node node stream ""))
  ([node stream prefix]
   (binding [*out* stream]
     (print (format "%n<%s%s,{" prefix (:datum @node)))
     (doseq [e (:label @node)]
       (env-string e))
     (print "}>"))))

(defn why-nodes
  ([atms] (why-nodes atms *out*))
  ([atms stream]
   (doseq [n (reverse (:nodes @atms))]
     (why-node n stream))))

(defn node-justifications
  ([node] (node-justifications node *out*))
  ([node stream]
   (binding [*out* stream]
     (println (format "%n For %s:" (node-string @node)))
     (doseq [j (:justs @node)]
       (print-justification j stream)))))

(defn print-justification
  ([j] (print-justification j *out*))
  ([j stream]
   (binding [*out* stream]
     (print (format "%n  %s, " (:informant @j)))
     (doseq [a (:antecedents @j)]
       (why-node a stream "     ")))))

(defn e [atms n]
  (some (fn [[_ bucket]]
          (some (fn [env]
                  (when (= (:index @env) n)
                    env))
                bucket))
        (:env-table @atms)))

(defn print-env
  ([env] (print-env env *out*))
  ([env stream]
   (binding [*out* stream]
     (print (format "%n%s:%s" (print-str @env) (if (:nogood? @env) "* " " ")))
     (env-string env))))

(defn env-string
  ([e] (env-string e *out*))
  ([e stream]
   (let [assumptions (:assumptions @e)
         strings (when (seq assumptions)
                   (let [printer (:node-string @(:atms @(first assumptions)))]
                     (sort (map #(printer @%) assumptions))))]
     (binding [*out* stream]
       (print (format "{~{~A~^,~}}" strings))))))

;;; Printing global data

(defn print-nogoods
  ([atms] (print-nogoods atms *out*))
  ([atms stream]
   (print-env-table (:nogood-table @atms) stream)))

(defn print-envs
  ([atms] (print-envs atms *out*))
  ([atms stream]
   (print-env-table (:env-table @atms) stream)))

(defn print-env-table [table stream]
  (doseq [[_ bucket] table]
    (doseq [env bucket]
      (print-env env stream))))

(defn print-atms-statistics [atms]
  (print-table (format "%n For env table:") (:env-table @atms))
  (print-table (format "%n For nogood table:") (:nogood-table @atms)))

(defn print-table [msg table]
  (println msg)
  (doseq [[length bucket] table]
    (println (format "   Length %d, %d" length (count bucket)))))

(defn print-atms-summary [atms]
  "Print a summary of ATMS statistics"
  (let [atms-data @atms
        total-nodes (count (:nodes atms-data))
        total-assumptions (count (:assumptions atms-data))
        total-justs (count (:justs atms-data))
        total-nogoods (count (mapcat second (:nogood-table atms-data)))
        total-envs (count (mapcat second (:env-table atms-data)))]
    (println "\n=== ATMS Summary ===")
    (println (format "Title:        %s" (:title atms-data)))
    (println (format "Nodes:        %d (%d assumptions)" total-nodes total-assumptions))
    (println (format "Justifications: %d" total-justs))
    (println (format "Environments: %d" total-envs))
    (println (format "Nogoods:      %d" total-nogoods))
    (println "===================\n")))
