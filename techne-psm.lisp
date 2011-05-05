;;; The Problem Solver Module for Techne.
;;; Contains the interface described in papers/caise11-tms/caise-tms.tex
;;; E.g. ASK, TELL, UNTELL

;;; Load the ATMS
(load "/Users/nernst/Dropbox/research/src/tms/techne-atms.dx32fsl") ; Clozure
;(load "techne-atms.fasl") ; SBCL

;;; Data storage
;;; TODO: figure out why defclass is preferred for REKB over defstruct
(defvar *goals* ())
(defvar *assumptions* ())
(defvar *opts* ()) ; optional goals
(defvar *assm-count* 0) ; how many assumptions have we been told? The computational limit is ~300
(defvar *mands* ())  ; use equal to compare strings (?)
(defvar *assm-form* ()) ; hold labelled formulae
(defvar *labels* (make-hash-table :test 'equalp)) ;case insensitive
(defvar *impl-repo* nil) ; hold the sets of implementations

;;;Distance functions
(defun min-effort (new-solutions)
  " The solution which differs in the fewest tasks"
  ;; TODO add minimize set size of new solution
  (let ((existing (getf (first *impl-repo*) :TASKS )) (smallest (first new-solutions)))
    ;;assume use last set of impl
    (loop for soln in new-solutions
       if (< (length (set-difference soln existing)) (length (set-difference smallest existing)))
         do (setf smallest soln)
       finally (return smallest))))
  )

(defun max-fam (new-solutions)
  "The solution which differs in the fewest tasks from the previous one"
  ;; maximize intersection
  (let ((existing (getf (first *impl-repo*) :TASKS )) (biggest (first new-solutions)))
    ;;assume use last set of impl
    (loop for soln in new-solutions
         ;do (print existing)
       if (> (length (intersection existing soln)) (length (intersection existing biggest)))
         do (setf biggest soln)
       finally (return biggest))))

(defun simple (new-solutions)
  "The shortest new solution, if a match, determined non-deterministically"
  (let ((sorted (sort new-solutions #'shorterp)))
    (first sorted)))

(defun shorterp (list1 list2)
  (< (length list1) (length list2)))
  
;; track implemented tasks
(defun mark-implemented (tasks &optional (name "") )
  "Take a list of tasks which the user marked as selected"
  ;; TODO: verify they are actually a valid solution?
  ;; TODO: store old goals .. where do they go.
  (push (list :name name :tasks tasks :date (get-universal-time)) *impl-repo*))
 
;;; TELL/UNTELL
(defun declare-atomic (atom label sort rekb)
  "Add the pair (ATOM LABEL) to the symbol table"
  ;; if a task or DA, we justify with an assumption to indicate defeasibility
  (if (or (eql sort :DA) (eql sort :TASK)) 
      (progn
        (let  ((ass-node (tms-create-node rekb label :ASSUMPTIONP nil)))
        (let ((fakenode (tms-create-node rekb (concatenate 'string "fake-" label)  :ASSUMPTIONP t)))
          (incf *assm-count*)
          (justify-node (concatenate 'string "fake-just-" label) ass-node (list fakenode))
          ;(setf *assumptions* (nconc *assumptions* (list label))) ; the name
          (setf (gethash  (concatenate 'string "fake-" label) *labels*) fakenode) 
          ;(nconc *assumption-refs* (list fakenode))) ; the actual node
          ass-node)))
      (progn ;; otherwise, just a simple node
        (setf *goals* (nconc *goals* (list label)))
        (tms-create-node rekb label :ASSUMPTIONP nil))))

(defun undeclare-atomic (label rekb)
  "Remove ATOM and any FORMULAs with ATOM as member from REKB."
  ;; see DeKleer's ATMS paper for the default logic reasoning behind this
  ;; lookup the label in the list of DA or TASKs
  (unless (label-in-table-p (concatenate 'string "fake-" label))
    (error "\"~A\" is not a valid atom" label))
  (let ((x (gethash (concatenate 'string "fake-" label) *labels*)))
    (justify-node (concatenate 'string "retracted-" label) (contradiction rekb) (list x))
    (remhash (concatenate 'string "fake-" label) *labels* )))
  
(defun assert-formula (consequent ants sort label rekb)
  "If equivalent formula not yet asserted, add to set of LABELLED SORTED FORMULA."
  ;;TODO ant/cons should be a single arg formula
  ;; create a fake node that will be used to retract the formula.
  (when (eq sort :DA)
    (let ((ass-node (tms-create-node rekb (concatenate 'string "fakeform-" label) :ASSUMPTIONP t)))
      ;; call the ATMS justification method from fake-node + ants to consequent
      ;; add this node to the table for future retraction
      (setf (gethash  (concatenate 'string "fakeform-" label) *labels*) ass-node) 
      (let ((new-ants (append (list ass-node) ants)))
        (justify-node label consequent new-ants)))))

(defun retract-formula (label rekb)
  "Remove formula identified by LABEL from REKB."
  (unless (label-in-table-p (concatenate 'string "fakeform-" label))
    (error "\"~A\" is not a valid formula" label))
  (let ((x (gethash (concatenate 'string "fakeform-" label) *labels*)))
    (justify-node (concatenate 'string "retractedform-" label) (contradiction rekb) (list x))
    (remhash (concatenate 'string "fakeform-" label) *labels* )))

(defun assert-opt (goal rekb)
  "Flag ATOM as attractive."
  ;; todo: restrict so can only be performed on goals
  (setf *opts* (nconc *opts* (list goal))))

(defun retract-opt (goal rekb)
  "Remove the optional status of this goal"
  (setf *opts* (remove goal *opts*)))

(defun assert-mandatory (goal rekb mandatory?) 
  "Flag ATOM as mandatory or not mandatory in solution."
  ;; get the goal name
  ;; (setq goal-name (node-string goal))
  ;; (if mandatory?
  ;;     (setq value goal)
  ;;     (setq value nil))
  ;; (setf (gethash goal-name *mand*) value)
  ;; )
  (setf *mands* (nconc *mands* (list goal))))
;; add to the list, hash is quicker but don't think it will matter

(defun retract-mand (goal rekb)
  "Remove the optional status of this goal"
  (setf *mands* (remove goal *mands*)))

                                        ;ASKs
(defun ask-sort (label rekb)
  "Return the sort of the given label"
  )

(defun is-solution-p (rekb)
  "Check whether all mandatory goals are satisfied by a common explanation"
  )

(defun are-goals-achieved-from-p (goals rekb)
  "For the given list of goals, are all the mandatory goals explained by that set?"
  )

(defun are-goals-entailed-p (goals rekb)
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
        (when (not (not (compare-env env other-env))) ; if EQ,:S21,:S12 continue
            (setf all-false? nil)))
      ;; looped all envs in that goal, should we continue?
      (if all-false?
          (setf congruent? nil))) ;TODO add short-circuit to prevent eval of all goals.
    (if congruent?
        (setf entailed? t))) ;at least one env in the other goals matches this env.
  ;;TODO again short-circuit would be nice.
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

(defun print-rekb (rekb)
  "print the values in the rekb"
  (print *assumptions*)
  (print-atms-statistics rekb))

(defun print-assumption-hash (rekb)
  "print the keys of the assumption nodes added for retraction"
  (loop for key being the hash-keys of *labels*
     do (print key)))

(defun label-in-table-p (label)
  "try to find the given label, suitably prepended with 'fake-' or 'fakeform-', in the dictionary"
  (nth-value 1 (gethash label *labels*)))
