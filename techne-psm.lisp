;;; The Problem Solver Module for Techne.
;;; Contains the interface described in techne/papers/caise11-tms/caise-tms.tex
;;; E.g. ASK, TELL, UNTELL

;;; Load the ATMS
(load "techne-atms.dx64fsl") ; Clozure
;(load "techne-atms.fasl") ; SBCL

;;; Data storage
;;; TODO: figure out why defclass is preferred for REKB over defstruct
(defvar *goals* nil)
(defvar *da* nil)
(defvar *tasks* nil)
(defvar *opts* nil) ; optional goals
(defvar *assm-count* 0) ; how many assumptions have we been told? The computational limit is ~100? TODO verify this.

;;;Distance functions
(defun min-effort (initial-solution new-solutions)
  " The solution which differs in the fewest tasks and is shortest."
  )

(defun max-fam (initial-solution new-solutions)
  "The solution which differs in the fewest tasks from the previous one"
  )

(defun simple (initial-solution new-solutions)
  "The shortest new solution"
  )

;;; TELLs
(defun declare-atomic (atom label sort rekb)
  "Add the pair (ATOM LABEL) to the symbol table"
                                        ; was it a task?
  (if (eql sort :TASK)
      (setf ?assume t) ;TODO also add this to list of goals etc.
      (setf ?assume nil))
  (tms-create-node rekb label :ASSUMPTIONP ?assume))

(defun assrt-formula (consequent ants sort label rekb) ;TODO ant/cons should be a single arg formula
  "If equivalent formula not yet asserted, add to set of LABELLED SORTED FORMULA."
  (justify-node label consequent ants)
  )

(defun assrt-opt (goal rekb) ; changed from assert to assrt to avoid conflict with lisp-unit
  "Flag ATOM as attractive."
  )

                                        ;UNTELLs
(defun undeclare-atomic (atom rekb)
  "Remove ATOM and any FORMULAs with ATOM as member from REKB."
  )

(defun retract-formula (label rekb)
  "Remove formula identified by LABEL from REKB."
  )

(defun retract-opt (goal rekb)
  "Remove the optional status of this goal"
  )

                                        ;ASKs
(defun ask-sort (label rekb)
  "Return the sort of the given label"
  )

(defun are-goals-entailed? (goals rekb)
  "Are the set of goals entailed by the rekb?
  entailment of a set of goals mean they are consistent w/ all of at
  least 1 of the other goal's contexts"
  
  (setf goal (pop goals))  ;get environments for each goal 
  (setf entailed? nil) ; TODO use LET instead of setf?
  (dolist (env (tms-node-label goal)) ;env of first goal (tms-node-label goal) 
    (setf congruent? t)
    (dolist (other goals) ; compare to remaining goals
      (setf all-false? t)
      (dolist (other-env (tms-node-label other))
        (if (not (not (compare-env env other-env))) ; if EQ,:S21,:S12 continue
            (setf all-false? nil)))
      ;; looped all envs in that goal, should we continue?
      (if all-false?
          (setf congruent? nil))) ;TODO add short-circuit to prevent eval of all goals.
    (if congruent?
        (setf entailed? t))) ;at least one env in the other goals matches this env. TODO again short-circuit would be nice.
  entailed?) 

(defun min-change-task-entailing (goals tasks dist_fn rekb)
  "Return the set of sets of tasks which are a minimal change from the existing solution,
   as defined in DIST FN, and entail the given set of GOALs. If Set(TASK) is ∅,
   then this is the first implementation."
  )

(defun min-goal-achieve (goals rekb)
  " return the (minimal) sets of envs which entail the goals"
                                       
  (setf entail-sets ())    ; declare a list called entail_sets which holds result
  (setf pivot (pop goals))  ; select a pivot node
  (dolist (env (tms-node-label pivot)) ; for each env in that pivot
    (setf meet-env env)  ; make that env the meet_env
    (setf add-env? t)  

    (dolist (other goals) 
      (setf all-false? t)
      (dolist (other-env (tms-node-label other))   ; for each env in that goal find at least one match                                      
        (setf outcome (compare-env meet-env other-env))   ; check if env is sup/subset or = to meet_env
        ;(format t "~%comparison: ~a for nodes ~a and ~a" outcome meet-env other-env) 
        (cond ((eq :EQ outcome) (setf all-false? nil)) 
              ((eq :S12 outcome) (progn (setf meet-env other-env)
                                        (setf all-false? nil))) ; if t then that becomes meet env 
               ((eq :S21 outcome) (setf all-false? nil))
               ((eq nil outcome) ())))
      (if all-false?
          (setf add-env? nil))) ;all envs in this goal were not a match, so the pivot env is invalid.
    (if add-env? (pushnew meet-env entail-sets))) ; add to the set of acceptable envs 
  entail-sets)

(defun get-max-optional-solutions (tasks rekb)
  "Return sets of sets of tasks such that those tasks maximize the
   number of options they satisfy. The options are defined as those elements
   which were ASSERTED OPT."
  ;; use node-consistent-with to give new environments and check if not NOGOOD
  ;; the sets of sets of tasks are assumed to be the first solution you had.
  ;; e.g. (gmos ((list t1 t2) (list t2 t3)) *rekb*)
  )

;;; Helper functions
(defun create-rekb (name)
  "Eventually will create a REKB object, which holds lists of goals etc."
  (create-atms name :DEBUGGING nil))

(defun sorted-formula (sort consequent &rest antecedents)
  (list consequent (first antecedents) (rest antecedents)))

(defun contradiction (rekb)
  "return the TMS contradiction special node"
  (tms-create-node rekb 'CONTRADICTION :CONTRADICTORYP t))
