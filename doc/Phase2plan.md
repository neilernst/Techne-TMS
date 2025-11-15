# Phase 2: Parallelize Weave Function - Implementation Plan

## Overview

Phase 2 implements actual parallelization in the `weave` function, which is the highest-impact bottleneck in ATMS. The weave function combines environment labels from multiple antecedent nodes, creating a combinatorial explosion that benefits significantly from parallel processing.

**Goals:**
- Parallelize environment processing in weave function
- Achieve 2-3x speedup on 4-core systems, 3-4x on 8-core
- Maintain correctness (all existing tests pass)
- Use threshold-based activation (only for large workloads)

**Estimated Time:** 2-3 hours
**Risk Level:** Medium (modifies core algorithm, but preserves logic)

---

## Current Weave Implementation Analysis

### Location
`src/clojure/techne_atms.clj` lines 291-314

### Current Algorithm
```clojure
(defn weave [antecedent envs antecedents]
  (loop [remaining-antecedents (remove #{antecedent} antecedents)
         current-envs envs]
    (if (empty? remaining-antecedents)
      current-envs
      (let [node (first remaining-antecedents)
            next-envs (atom [])]
        (doseq [env current-envs]                    ;; OUTER LOOP - Parallelize here
          (doseq [node-env (:label @node)]           ;; INNER LOOP
            (let [new-env (union-env env node-env)]
              (when-not (:nogood? @new-env)
                (let [subsumed? (atom false)]
                  (doseq [existing-env @next-envs]   ;; SUBSUMPTION CHECK
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
```

### Complexity Analysis
- **Time Complexity:** O(A × E × L × N) where:
  - A = number of antecedents
  - E = number of current environments
  - L = average label size per node
  - N = subsumption check overhead
- **Parallelization Target:** Outer loop over `current-envs` (E dimension)

### Bottlenecks
1. **Nested loops:** env × node-label × subsumption checks
2. **Subsumption checking:** Sequential, per-environment
3. **Combinatorial explosion:** Grows exponentially with antecedents

---

## Implementation Strategy

### Step 1: Extract Subsumption Logic (Helper Function)

**Goal:** Separate subsumption checking from main logic for clarity and testability

**Location:** Add after `compare-env` function (around line 280)

**Code to Add:**
```clojure
(defn remove-subsumed-envs
  "Remove subsumed environments from a collection.

   For each new environment, checks if it is:
   - Subsumed by any existing env (discard it)
   - Subsumes any existing env (remove those)

   Args:
     envs - Collection of environment atoms

   Returns:
     Vector of non-subsumed environments"
  [envs]
  (reduce (fn [acc new-env]
            (let [subsumed? (atom false)
                  remaining (atom [])]
              ;; Check if new-env is subsumed or subsumes existing
              (doseq [existing-env acc]
                (when-not @subsumed?
                  (when-let [comparison (compare-env new-env existing-env)]
                    (case comparison
                      :EQ (reset! subsumed? true)
                      :S21 (reset! subsumed? true)
                      :S12 nil  ;; existing-env is subsumed, don't add it
                      (swap! remaining conj existing-env)))
                  (when (nil? (compare-env new-env existing-env))
                    (swap! remaining conj existing-env)))))
              (if @subsumed?
                acc
                (conj @remaining new-env))))
          []
          envs))
```

**Testing:**
```clojure
;; Test with simple environments
(let [atms (create-atms "Test")
      e1 (create-env atms [])
      e2 (create-env atms [])
      e3 (create-env atms [])]
  (remove-subsumed-envs [e1 e2 e3]))
;; Should return all three if none subsume each other
```

---

### Step 2: Create Parallel Weave Worker Function

**Goal:** Extract the per-environment processing logic

**Location:** Before `weave` function (around line 290)

**Code to Add:**
```clojure
(defn process-env-with-node
  "Process a single environment against all labels of a node.

   Args:
     env - Environment atom to process
     node - Node atom whose labels to combine with env

   Returns:
     Vector of new non-nogood environments created by union operations"
  [env node]
  (reduce (fn [acc node-env]
            (let [new-env (union-env env node-env)]
              (if (:nogood? @new-env)
                acc
                (conj acc new-env))))
          []
          (:label @node)))
```

**Testing:**
```clojure
;; Test with simple node and environment
(let [atms (create-atms "Test")
      n1 (tms-create-node atms "N1" {:assumption? true})
      env (create-env atms [n1])]
  (process-env-with-node env n1))
;; Should return vector of environments
```

---

### Step 3: Implement Parallel Weave Function

**Goal:** Replace sequential weave with parallel version

**Location:** Replace existing `weave` function (lines 291-314)

**Code to Replace:**
```clojure
(defn weave [antecedent envs antecedents]
  (let [atms (:atms @antecedent)]
    (loop [remaining-antecedents (remove #{antecedent} antecedents)
           current-envs envs]
      (if (empty? remaining-antecedents)
        current-envs
        (let [node (first remaining-antecedents)
              node-labels (:label @node)

              ;; Decide whether to use parallel processing
              use-parallel? (should-parallelize? atms (count current-envs))

              ;; Process all environments (parallel or sequential)
              new-envs-nested (if use-parallel?
                                ;; PARALLEL PATH
                                (pmap (fn [env]
                                        (process-env-with-node env node))
                                      current-envs)
                                ;; SEQUENTIAL FALLBACK
                                (map (fn [env]
                                       (process-env-with-node env node))
                                     current-envs))

              ;; Flatten results and remove subsumed environments
              all-new-envs (apply concat new-envs-nested)
              next-envs (remove-subsumed-envs all-new-envs)]

          (if (seq next-envs)
            (recur (rest remaining-antecedents) next-envs)
            []))))))
```

**Key Changes:**
1. Added `atms` extraction for `should-parallelize?` check
2. Replaced nested `doseq` with `pmap`/`map` + helper function
3. Extract subsumption to separate function
4. Flatten nested results with `(apply concat ...)`
5. Threshold-based parallel activation

---

### Step 4: Add Debugging/Reporting Support

**Goal:** Track when parallelization is used for performance analysis

**Location:** Inside the `weave` function, after the `use-parallel?` decision

**Code to Add:**
```clojure
;; Add after the use-parallel? binding:
(when (and use-parallel? (:reporting @atms))
  (println (format "  [PARALLEL] Weaving %d envs × %d labels (node: %s)"
                   (count current-envs)
                   (count node-labels)
                   (node-string node))))
```

---

## Testing Strategy

### Unit Tests

Create test file: `src/clojure/techne_atms_weave_test.clj`

```clojure
(ns techne-atms-weave-test
  (:require [clojure.test :refer :all]
            [techne-atms :refer :all]))

(deftest test-process-env-with-node
  (testing "Process single environment with node labels"
    (let [atms (create-atms "Test")
          n1 (tms-create-node atms "A1" {:assumption? true})
          n2 (tms-create-node atms "A2" {:assumption? true})
          env1 (create-env atms [n1])]
      ;; Process env1 with n2's labels
      (let [result (process-env-with-node env1 n2)]
        (is (vector? result))
        (is (every? #(not (:nogood? @%)) result))))))

(deftest test-remove-subsumed-envs
  (testing "Remove subsumed environments from collection"
    (let [atms (create-atms "Test")
          n1 (tms-create-node atms "A1" {:assumption? true})
          n2 (tms-create-node atms "A2" {:assumption? true})
          e1 (create-env atms [n1])
          e2 (create-env atms [n1 n2])
          e3 (create-env atms [n2])]
      ;; e2 subsumes e1 (e2 has more assumptions)
      (let [result (remove-subsumed-envs [e1 e2 e3])]
        (is (vector? result))
        ;; Result should contain non-subsumed environments
        (is (pos? (count result)))))))

(deftest test-weave-parallel-correctness
  (testing "Parallel weave produces same results as sequential"
    ;; Create problem with known structure
    (let [atms-seq (create-atms "Sequential" :parallel? false)
          atms-par (create-atms "Parallel" :parallel? true :parallel-threshold 1)

          ;; Create identical node structures
          nodes-seq (doall (repeatedly 5 #(tms-create-node atms-seq (gensym "N") {:assumption? true})))
          nodes-par (doall (repeatedly 5 #(tms-create-node atms-par (gensym "N") {:assumption? true})))

          ;; Create justifications
          result-seq (tms-create-node atms-seq "Result-Seq")
          result-par (tms-create-node atms-par "Result-Par")

          just-seq (justify-node "J-Seq" result-seq nodes-seq)
          just-par (justify-node "J-Par" result-par nodes-par)]

      ;; Both should produce same number of environments
      (is (= (count (:label @result-seq))
             (count (:label @result-par)))))))

(deftest test-weave-threshold
  (testing "Weave respects parallel threshold"
    (let [atms (create-atms "Test" :parallel? true :parallel-threshold 100)
          n1 (tms-create-node atms "A1" {:assumption? true})
          n2 (tms-create-node atms "A2" {:assumption? true})]
      ;; With small label size, should use sequential
      (let [result (weave n1 (:label @n1) [n1 n2])]
        (is (coll? result))))))
```

### Integration Testing

**Test with existing test suite:**
```bash
# Run all existing tests
clojure -M:test

# Run specific test file
clojure -M -e '(load-file "src/clojure/techne_atms_test.clj") (clojure.test/run-tests)'
```

**Expected Results:**
- All existing tests pass unchanged
- No behavioral differences
- Parallel code paths exercised with reporting enabled

---

### Performance Verification

**Run benchmark suite to measure improvement:**

```bash
# Run benchmarks with Phase 2 implementation
cd src/clojure
clojure -M -e '(load-file "benchmark.clj") (in-ns (quote benchmark)) (run-performance-analysis)'

# Generate visualizations
cd ../..
source venv/bin/activate
python3 plot_benchmarks.py src/clojure/benchmark_results.csv
```

**Expected Improvements (Phase 1 → Phase 2):**
- Small problems (25-50 nodes): 1.2-1.5x → 1.5-2.0x speedup
- Medium problems (75-100 nodes): 1.3-1.5x → 2.0-3.0x speedup
- Large problems (150 nodes): 1.4x → 2.5-3.5x speedup
- Best case (8 cores): Up to 4x speedup

---

## File Changes Summary

### Files to Modify
1. `src/clojure/techne_atms.clj` - Implement parallel weave

### Files to Create
1. `src/clojure/techne_atms_weave_test.clj` - Unit tests for weave parallelization
2. `doc/Phase2plan.md` - This file (implementation plan)

### Files to Run (verification)
1. Existing test suite
2. `src/clojure/benchmark.clj` - Performance comparison

---

## Rollback Plan

If Phase 2 causes issues:

1. **Immediate Rollback:**
   ```bash
   git checkout src/clojure/techne_atms.clj
   ```

2. **Verification:**
   - Run existing test suite
   - Confirm all tests pass
   - Verify no functionality broken

3. **Debugging:**
   - Check parallel threshold settings
   - Verify `pmap` behavior with small collections
   - Test `remove-subsumed-envs` independently
   - Enable reporting to see parallel activation

---

## Success Criteria

Phase 2 is complete when:

- [ ] `process-env-with-node` helper function implemented
- [ ] `remove-subsumed-envs` helper function implemented
- [ ] `weave` function parallelized with threshold check
- [ ] Reporting/debugging support added
- [ ] All new unit tests pass
- [ ] All existing tests pass (regression check)
- [ ] Benchmark suite shows performance improvement
- [ ] Performance gains documented (2-3x on 4-core, 3-4x on 8-core)
- [ ] Code compiles without warnings
- [ ] Documentation comments updated
- [ ] No correctness regressions (same results as Phase 1)

---

## Performance Expectations

### Theoretical Analysis

**Sequential Complexity:** O(A × E × L × N)
- A = antecedents (typically 2-5)
- E = environments (grows exponentially, can reach 100-1000)
- L = label size per node (typically 1-10)
- N = subsumption check (O(E) in worst case)

**Parallel Complexity:** O(A × E/P × L × N)
- P = number of cores
- Parallelizes over E dimension
- Amdahl's Law applies (subsumption is sequential)

**Expected Speedup:**
- 4 cores: 2.0-3.0x (75-80% parallel fraction)
- 8 cores: 3.0-4.0x (85-90% parallel fraction)
- Diminishing returns beyond 8 cores due to subsumption overhead

---

## Risk Mitigation

### Potential Issues and Solutions

1. **Issue:** Race conditions in subsumption checking
   - **Solution:** Each worker produces independent results; merge is sequential

2. **Issue:** Memory overhead from parallel processing
   - **Solution:** Threshold prevents parallel activation on small problems

3. **Issue:** `pmap` creates lazy sequence causing unexpected behavior
   - **Solution:** Force evaluation with `doall` if needed: `(doall (pmap ...))`

4. **Issue:** Performance degradation on small problems
   - **Solution:** `should-parallelize?` guards against this

5. **Issue:** Different results between parallel and sequential
   - **Solution:** Order-independent algorithms; extensive testing

---

## Code Quality Checklist

Before considering Phase 2 complete:

- [ ] All functions have docstrings
- [ ] Edge cases handled (empty envs, single node, nogoods)
- [ ] Input validation where appropriate
- [ ] Consistent naming conventions
- [ ] No magic numbers (use threshold from config)
- [ ] Error messages clear and actionable
- [ ] Code follows Clojure style guide
- [ ] No compiler warnings
- [ ] Helper functions tested independently

---

## Timeline

**Total Estimated Time:** 2-3 hours

| Task | Estimated Time | Dependencies |
|------|---------------|--------------|
| Read and understand weave | 20 min | - |
| Implement remove-subsumed-envs | 30 min | - |
| Implement process-env-with-node | 20 min | - |
| Modify weave function | 40 min | Above helpers |
| Add debugging/reporting | 10 min | Modified weave |
| Write unit tests | 40 min | All above |
| Run tests and verify | 20 min | Tests written |
| Run benchmarks | 20 min | Tests pass |
| Compare performance | 10 min | Benchmarks complete |
| Documentation | 10 min | - |

---

## Approval Checklist

Before proceeding with implementation:

- [ ] Plan reviewed and approved
- [ ] Understanding of weave algorithm
- [ ] Test strategy agreed upon
- [ ] Performance expectations realistic
- [ ] Rollback plan understood
- [ ] Success criteria clear

**Ready to implement?** [YES / NO / NEEDS REVISION]
