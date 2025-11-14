;;;;
;;;; Hold the graph datastructure of the problem, do some clean ups
;;;; Author: Neil Ernst
;;;; Last updated: May 2011
;;;; TODO: add contradiction handling; speed up AND-Subtree component; automatically collapse subtrees 
;;;;

                                        ; import the library for graphs
(ql:quickload "cl-graph")
                                        ; move to package
(in-package #:cl-graph)
                                        ; make the graph object
                                        ; make user do this
;(defvar *g* (METABANG.CL-CONTAINERS::make-container 'cl-graph:graph-container :default-edge-type :directed))
(defvar *imp-num* 0)

(defun make-graph (name)
  (defvar *g* (METABANG.CL-CONTAINERS::make-container 'cl-graph:graph-container :default-edge-type :directed)))
  
;;(add-techne-edge 'g1 (list 'g2 'g3 'g4) "implication" *g*) ; single and-subtree
;;(add-techne-edge 'g1 (list 'g6 'g7 'g8) "implication" *g*) ; single and-subtree
(defun add-techne-edge (dest srcs type graph)
  ;; imp-num is current implication pseudo-node
  (let ((cur-imp (concatenate 'string type (write-to-string (incf *imp-num*)))))
    (loop for src in srcs do
         (cl-graph:add-edge-between-vertexes graph src cur-imp ))
    (cl-graph:add-edge-between-vertexes graph cur-imp dest)))

(defun and-subtree-p (graph vertex)
  "does the given vertex have or-children or contradictions?"
  ;; todo: use edgelist to add short-circuit, add contradiction detection
  (let ((kids (dfs graph vertex #'nothing))) ;; kids = list of child nodes
    (loop for kid in kids
       ;do (print kid)
       ;do (print (target-edge-count  kid))
       if (and (regular-node-p kid)
               (not (taskp kid))
               (> (target-edge-count kid) 1)) ;; a non-meta node, that is not a task, with more than one incoming arc, is an OR node
         return nil
       finally (return t)))) ; we reached the end, and no OR nodes found.

(defun find-and-subtree-roots (graph)
  "account for our intermediate implication nodes. Does not account for alternatives.
   In other words, the model ((a^b)v(c^d)) -> e will return a,b,c,d"
  (let ((children (gather-non-link-nodes graph))) ; 'parents' are what I think of as children.
    (loop for child in children
       if (and (and-subtree-p graph child)
               (not (taskp child)))
        ; do (progn (format t "~%The following node is a root for an AND_Subtree")
                                        ;          (print child))
       collect child into subtrees
       finally (return subtrees))))

(defun collect-and-tasks (graph)
  "for the given graph, return sets of tasks which can be collapsed into one meta-task"
  ;; todo: add contradiction handling
  (let ((leaves (find-leaves graph)))
    (let ((imps (find-leaf-implications leaves)))
      (loop for impl in imps
         collect (parent-vertexes impl) into tasks
         finally (return tasks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-leaf-implications (leaves)
  "Find the implication nodes to which the leaves are connected"
  (loop for leaf in leaves
     append (child-vertexes leaf) into implications
     finally (return (make-set implications))))
       
(defun taskp (vertex)
  "return true if this vertex a task/leaf node"
  (eq 0 (target-edge-count vertex)))

(defun nothing (arg) "a pass function")

(defun stats (graph)
  "Print number of nodes and edges for graph"
  (format t "~%Vertices: ~A" (length (cl-graph:vertexes graph)) )
  (format t "~%Edges: ~A" (length (cl-graph:edges graph)) )
  )

(defun find-leaves (graph)
  "Find all leaves for the graph, leaf being a node with no out-edges"
  (loop for v in (vertexes graph)
     if (= (target-edge-count v) 0) collect v into leaves
     finally (return leaves)))

(defun find-roots* (graph)
  "Find all roots of the graph, root being a node with no in-edges"
  (loop for v in (vertexes graph)
     if (= (source-edge-count v) 0) collect v into roots
     finally (return roots)))

(defun gather-non-link-nodes (graph)
  (loop for v in (vertexes graph)
     if (regular-node-p v) collect v into nodes
     finally (return nodes)))

(defun regular-node-p (v)
  "return true if this node v is not a meta-node for implication/contradiction"
  (and (not (search "contradiction" (string (slot-value v 'element))))
           (not (search "implication" (string (slot-value v 'element)))) ))

(defun find-roots (graph)
  (graph-roots graph))

(defun find-root (graph)
  "If the graph is a tree, return the single root"
  (car (find-roots graph)))

(defun make-dot (graph)
  (cl-graph:graph->dot graph t))

(defun make-set (list-in &optional (list-out '()))
  "courtesy of http://stackoverflow.com/questions/165767/does-common-lisp-have-a-something-like-javas-set-interface-implementing-classes/238789#238789"
  (if (endp list-in)
      (nreverse list-out)
      (make-set
        (cdr list-in)
        (adjoin (car list-in) list-out :test 'equal))))

;;;;;;;;;;;;;
;;; Tests ;;;
;;;;;;;;;;;;;
(defun test ()
  (let ((g (METABANG.CL-CONTAINERS:make-container 'graph-container :default-edge-type :directed)))
    (loop for (src dst) in '((a b) (a c) (c d) (a d) (d e) (e f) (b f)) do
         (cl-graph:add-edge-between-vertexes g src dst))   
    (format t "~%Neighbors of vertex A:")
    (cl-graph:iterate-edges (find-vertex g 'a) #'print)))

(defun test-simple () ;; no shared nodes
  ;; TODO: this function should use techne calls
   (let ((ts (METABANG.CL-CONTAINERS:make-container 'graph-container :default-edge-type :directed)))
    (loop for (src dst) in '((3 i0) (4 i0) (2 i0) (i0 t0) (5 i1) (i1 t1) (t1 i3) (t0 i3) (i3 root)) do
         (cl-graph:add-edge-between-vertexes ts src dst))
    ;(format t "~%Initial graph:")
    (make-dot ts) ts)
   ;; print "trees:(2 4 3 5 t0 t1 :root root)"
   )

;;; another test should have shared nodes, e.g. 2-t1 and 2,3,4-t0
;;; another test should involve contradictions, e.g. t1 -> c
;; (setq ts (METABANG.CL-CONTAINERS:make-container 'graph-container :default-edge-type :directed))
;; (loop for (src dst) in '((3 i0) (4 i0) (2 i0) (i0 t0) (5 i1) (i1 t1) (t1 i3) (t0 i3) (i3 root)) do
;;      (cl-graph:add-edge-between-vertexes ts src dst))
