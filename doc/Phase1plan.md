# Phase 1: Parallelization Foundation - Implementation Plan

## Overview

Phase 1 establishes the infrastructure for multi-core parallelization without changing any algorithmic behavior. This phase is pure setup - adding configuration, CPU detection, and helper functions that later phases will use.

**Goals:**
- Add parallel configuration to ATMS
- Implement CPU core detection
- Create decision helpers for when to parallelize
- Zero impact on existing functionality (all tests pass unchanged)

**Estimated Time:** 1-2 hours
**Risk Level:** Low (additive changes only, no algorithm modifications)

---

## Current State Analysis

### File to Modify
`src/clojure/techne_atms.clj` (512 lines)

### Key Sections to Change

1. **ATMS Record Definition** (Line 13)
   ```clojure
   (defrecord ATMS [title node-counter just-counter env-counter nodes justs
                    contradictions assumptions debugging reporting nogood-table
                    contra-node env-table empty-env node-string enqueue-procedure])
   ```

2. **ATMS Creation Function** (Around line 55-80)
   - Currently: `create-atms` function initializes ATMS
   - Location needs to be verified by reading the file

3. **Namespace Declaration** (Lines 1-11)
   - May need to add imports for CPU detection

---

## Implementation Steps

### Step 1: Add CPU Core Detection Function

**Location:** After namespace declaration, before ATMS record (around line 45)

**Code to Add:**
```clojure
;;; Parallel processing utilities

(defn detect-cores []
  "Detect number of available CPU cores on this system.
   Returns the number of processors available to the JVM."
  (.availableProcessors (Runtime/getRuntime)))
```

**Testing:**
```clojure
;; Test in REPL
(detect-cores)  ;; Should return positive integer (e.g., 4, 8, 16)
```

**Success Criteria:**
- Function compiles without errors
- Returns integer > 0
- Value matches system CPU count

---

### Step 2: Extend ATMS Record

**Current (Line 13):**
```clojure
(defrecord ATMS [title node-counter just-counter env-counter nodes justs
                 contradictions assumptions debugging reporting nogood-table
                 contra-node env-table empty-env node-string enqueue-procedure])
```

**Modified:**
```clojure
(defrecord ATMS [title node-counter just-counter env-counter nodes justs
                 contradictions assumptions debugging reporting nogood-table
                 contra-node env-table empty-env node-string enqueue-procedure
                 ;; Parallel processing fields
                 parallel? num-cores parallel-threshold])
```

**New Fields:**
- `parallel?` - Boolean flag to enable/disable parallelization
- `num-cores` - Number of CPU cores available (auto-detected)
- `parallel-threshold` - Minimum collection size to trigger parallel processing

**Testing:**
```clojure
;; Should still be able to create records
(map->ATMS {:title "test" :parallel? true :num-cores 4 :parallel-threshold 10})
```

**Success Criteria:**
- ATMS record compiles without errors
- Can create instances with new fields
- Existing code using ATMS record still works

---

### Step 3: Add Parallelization Decision Helper

**Location:** After `detect-cores` function (around line 50)

**Code to Add:**
```clojure
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
```

**Testing:**
```clojure
;; Test decision logic
(let [atms (atom (map->ATMS {:parallel? true :num-cores 4 :parallel-threshold 10}))]
  (should-parallelize? atms 5)    ;; => false (below threshold)
  (should-parallelize? atms 15)   ;; => true (above threshold)
  (should-parallelize? atms 100)) ;; => true

(let [atms (atom (map->ATMS {:parallel? false :num-cores 4 :parallel-threshold 10}))]
  (should-parallelize? atms 100)) ;; => false (disabled)

(let [atms (atom (map->ATMS {:parallel? true :num-cores 1 :parallel-threshold 10}))]
  (should-parallelize? atms 100)) ;; => false (single core)
```

**Success Criteria:**
- Function compiles and runs
- Returns false when parallel? is false
- Returns false when collection-size < threshold
- Returns false when num-cores <= 1
- Returns true when all conditions met

---

### Step 4: Update ATMS Creation Function

**Current Function Location:** Need to find `create-atms` function

**Changes Required:**
1. Add optional parameters for parallel configuration
2. Auto-detect cores if not specified
3. Set sensible defaults

**Modified Function:**
```clojure
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
  [title & {:keys [debugging reporting parallel? num-cores parallel-threshold
                   node-string enqueue-procedure]
            :or {debugging false
                 reporting false
                 parallel? true
                 num-cores nil
                 parallel-threshold 10
                 node-string default-node-string
                 enqueue-procedure nil}}]
  (let [detected-cores (or num-cores (detect-cores))
        atms (atom (map->ATMS
                    {:title title
                     :node-counter 0
                     :just-counter 0
                     :env-counter 0
                     :nodes []
                     :justs []
                     :contradictions []
                     :assumptions []
                     :debugging debugging
                     :reporting reporting
                     :nogood-table {}
                     :env-table {}
                     :node-string node-string
                     :enqueue-procedure enqueue-procedure
                     ;; Parallel processing configuration
                     :parallel? parallel?
                     :num-cores detected-cores
                     :parallel-threshold parallel-threshold}))]
    ;; Create empty environment
    (let [empty-env (atom (map->Env {:index 0
                                     :count 0
                                     :assumptions []
                                     :nodes []
                                     :nogood? false
                                     :rules []}))]
      (swap! atms #(-> %
                      (assoc :empty-env empty-env)
                      (assoc :env-counter 1)
                      (assoc-in [:env-table 0] [empty-env]))))
    atms))
```

**Testing:**
```clojure
;; Test default creation
(let [atms (create-atms "Test ATMS")]
  (println "Parallel enabled:" (:parallel? @atms))  ;; => true
  (println "Cores detected:" (:num-cores @atms))    ;; => (detect-cores)
  (println "Threshold:" (:parallel-threshold @atms)) ;; => 10
  )

;; Test with custom settings
(let [atms (create-atms "Test ATMS" :parallel? false :parallel-threshold 20)]
  (println "Parallel enabled:" (:parallel? @atms))  ;; => false
  (println "Threshold:" (:parallel-threshold @atms)) ;; => 20
  )

;; Test backward compatibility (old code should still work)
(let [atms (create-atms "Old Style" :debugging true)]
  (println "Debugging:" (:debugging @atms))  ;; => true
  (println "Has parallel fields:" (contains? @atms :parallel?)) ;; => true
  )
```

**Success Criteria:**
- Function compiles without errors
- Auto-detects cores when not specified
- Respects custom parallel parameters
- Maintains backward compatibility
- All existing tests pass

---

### Step 5: Add Diagnostic/Reporting Function

**Location:** After helper functions (around line 60)

**Code to Add:**
```clojure
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
```

**Testing:**
```clojure
(let [atms (create-atms "Test" :parallel? true)]
  (println (parallel-config-string atms)))
;; => "Parallel processing: ENABLED (cores=8, threshold=10)"

(let [atms (create-atms "Test" :parallel? false)]
  (println (parallel-config-string atms)))
;; => "Parallel processing: DISABLED"
```

**Success Criteria:**
- Returns informative string
- Correctly reflects ATMS configuration

---

## Testing Strategy

### Unit Tests to Add

Create test file: `src/clojure/techne_atms_parallel_test.clj`

```clojure
(ns techne-atms-parallel-test
  (:require [clojure.test :refer :all]
            [techne-atms :refer :all]))

(deftest test-detect-cores
  (testing "CPU core detection"
    (let [cores (detect-cores)]
      (is (integer? cores))
      (is (> cores 0))
      (is (<= cores 256)))))  ;; Reasonable upper bound

(deftest test-should-parallelize
  (testing "Parallelization decision logic"
    (let [atms (create-atms "Test" :parallel? true :num-cores 4 :parallel-threshold 10)]
      ;; Below threshold
      (is (false? (should-parallelize? atms 5)))
      (is (false? (should-parallelize? atms 9)))

      ;; At or above threshold
      (is (true? (should-parallelize? atms 10)))
      (is (true? (should-parallelize? atms 100)))

      ;; Disabled
      (let [atms-disabled (create-atms "Test" :parallel? false)]
        (is (false? (should-parallelize? atms-disabled 100))))

      ;; Single core
      (let [atms-single (create-atms "Test" :parallel? true :num-cores 1)]
        (is (false? (should-parallelize? atms-single 100)))))))

(deftest test-atms-creation-with-parallel
  (testing "ATMS creation with parallel configuration"
    ;; Default creation
    (let [atms (create-atms "Test")]
      (is (= "Test" (:title @atms)))
      (is (true? (:parallel? @atms)))
      (is (integer? (:num-cores @atms)))
      (is (= 10 (:parallel-threshold @atms))))

    ;; Custom configuration
    (let [atms (create-atms "Custom"
                           :parallel? false
                           :num-cores 2
                           :parallel-threshold 20)]
      (is (false? (:parallel? @atms)))
      (is (= 2 (:num-cores @atms)))
      (is (= 20 (:parallel-threshold @atms))))

    ;; Backward compatibility
    (let [atms (create-atms "OldStyle" :debugging true :reporting true)]
      (is (true? (:debugging @atms)))
      (is (true? (:reporting @atms)))
      (is (contains? @atms :parallel?)))))

(deftest test-parallel-config-string
  (testing "Parallel configuration string generation"
    (let [atms (create-atms "Test" :parallel? true :num-cores 4)]
      (is (string? (parallel-config-string atms)))
      (is (.contains (parallel-config-string atms) "ENABLED"))
      (is (.contains (parallel-config-string atms) "cores=4")))

    (let [atms (create-atms "Test" :parallel? false)]
      (is (.contains (parallel-config-string atms) "DISABLED")))))
```

### Integration Testing

**Run Existing Test Suite:**
```bash
# Assuming you have a test runner
lein test

# Or with Clojure CLI
clojure -M:test
```

**Expected Results:**
- All existing tests pass unchanged
- No behavioral differences
- No performance regressions

**Manual Testing:**
```clojure
;; In REPL
(require '[techne-atms :refer :all])

;; Create ATMS and verify fields
(def atms (create-atms "Manual Test"))
(println "Configuration:")
(println (parallel-config-string atms))
(println)

;; Test with small collection (should not parallelize)
(println "Small collection (5 items):"
         (should-parallelize? atms 5))

;; Test with large collection (should parallelize)
(println "Large collection (100 items):"
         (should-parallelize? atms 100))

;; Test with disabled parallel
(def atms-seq (create-atms "Sequential" :parallel? false))
(println "Disabled config:"
         (should-parallelize? atms-seq 100))
```

---

## File Changes Summary

### Files to Modify
1. `src/clojure/techne_atms.clj` - Add parallel infrastructure

### Files to Create
1. `src/clojure/techne_atms_parallel_test.clj` - Unit tests for parallel utilities

### Files to Read (for context)
1. `src/clojure/techne_atms.clj` - Full file read to understand structure

---

## Rollback Plan

If Phase 1 causes issues:

1. **Immediate Rollback:**
   ```bash
   git checkout src/clojure/techne_atms.clj
   ```

2. **Verification:**
   - Run existing test suite
   - Confirm all tests pass
   - Verify no functionality broken

3. **Analysis:**
   - Review what went wrong
   - Check for missed dependencies
   - Verify all function signatures match

---

## Success Criteria

Phase 1 is complete when:

- [ ] `detect-cores` function implemented and tested
- [ ] ATMS record extended with 3 new fields
- [ ] `should-parallelize?` helper function working correctly
- [ ] `create-atms` updated with parallel parameters
- [ ] `parallel-config-string` diagnostic function added
- [ ] All new unit tests pass
- [ ] All existing tests pass (regression check)
- [ ] Manual REPL testing successful
- [ ] Code compiles without warnings
- [ ] Documentation comments added to all new functions
- [ ] No behavioral changes to existing functionality
- [ ] Configuration can be disabled (parallel? = false works)

---

## Next Steps (Post-Phase 1)

After Phase 1 is complete and verified:

1. **Code Review:** Review all changes for correctness
2. **Commit:** Create git commit with message: "Phase 1: Add parallelization infrastructure"
3. **Document:** Update main README with parallel configuration options
4. **Proceed:** Move to Phase 2 (weave function parallelization)

---

## Risk Mitigation

### Potential Issues and Solutions

1. **Issue:** Tests fail after ATMS record change
   - **Solution:** Ensure all record fields have default values in constructors

2. **Issue:** `detect-cores` returns unexpected value
   - **Solution:** Add bounds checking (min=1, max=256)

3. **Issue:** Backward compatibility broken
   - **Solution:** Make all new parameters optional with sensible defaults

4. **Issue:** Performance regression
   - **Solution:** Ensure `should-parallelize?` is fast (< 1μs per call)

---

## Code Quality Checklist

Before considering Phase 1 complete:

- [ ] All functions have docstrings
- [ ] Edge cases handled (nil values, zero cores, negative thresholds)
- [ ] Input validation where appropriate
- [ ] Consistent naming conventions
- [ ] No magic numbers (use named constants)
- [ ] Error messages are clear and actionable
- [ ] Code follows Clojure style guide
- [ ] No compiler warnings

---

## Timeline

**Total Estimated Time:** 1-2 hours

| Task | Estimated Time | Dependencies |
|------|---------------|--------------|
| Read existing code | 15 min | - |
| Add detect-cores | 10 min | - |
| Extend ATMS record | 10 min | - |
| Add helper functions | 20 min | ATMS record |
| Update create-atms | 30 min | All above |
| Write unit tests | 30 min | All above |
| Run tests and verify | 15 min | Tests written |
| Documentation | 10 min | - |
| Code review | 10 min | - |

---

## Approval Checklist

Before proceeding with implementation:

- [ ] Plan reviewed and approved
- [ ] Understanding of ATMS record structure
- [ ] Test strategy agreed upon
- [ ] Rollback plan understood
- [ ] Success criteria clear

**Ready to implement?** [YES / NO / NEEDS REVISION]
