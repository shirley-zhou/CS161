;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;


 ;; Breadth first search
(defun BFS(FRINGE)
  (cond ((null FRINGE) nil) ; base case
        ((atom FRINGE) FRINGE) ; base case
        ; if leftmost child is a leaf, immediately print and do BFS only on the rest:
        ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
        ; mimic a queue by placing leftmost child (car FRINGE) at end of list and calling BFS again:
        (T (BFS (append (cdr FRINGE) (car FRINGE))))))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (cond ((equal S '(T T T T)) T)
          (T nil)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poison and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  (cond ((equal A 'h)
        (cond ((or (equal (second S) (third S)) (equal (second S) (fourth S))) nil) ; invalid: baby and dog OR baby and poison on same side
              (T (list (append (list (not (first S))) (cdr S))))))
        ((equal A 'b)
        (cond ((not (equal (first S) (second S))) nil) ; impossible: homer not on same side as baby
              (T (list (append (list (not (first S))) (list (not (second S))) (cddr S))))))
        ((equal A 'd)
        (cond ((equal (second S) (fourth S)) nil) ; invalid: baby and poison on same side
              ((not (equal (first S) (third S))) nil) ; impossible: homer not on same side as dog
              (T (list (append (list (not (first S))) (list (second S)) (list (not (third S))) (list (fourth S)))))))
        ((equal A 'p)
        (cond ((or (equal (second S) (third S)) (equal (second S) (fourth S))) nil) ; invalid: baby and dog OR baby and poison on same side
              ((not (equal (first S) (fourth S))) nil) ; impossible: homer not on same side as poison
              (T (list (append (list (not (first S))) (list (second S)) (list (third S)) (list (not (fourth S))))))))
        (T nil))) ; invalid input

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond ((null STATES) nil)
          ((equal S (car STATES)) T)
          (T (ON-PATH S (cdr STATES)))))

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (cond ((null STATES) nil)
          ((DFS (car STATES) PATH) (DFS (car STATES) PATH)) ; if path exists, return it
          (T (MULT-DFS (cdr STATES) PATH)))) ; else try on rest of possible STATES

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond ((null S) nil)
          ((ON-PATH S PATH) nil) ; state S already visited
          ((FINAL-STATE S) (append PATH (list S))) ; arrived at final state
          (T (MULT-DFS (SUCC-FN S) (append PATH (list S))))))