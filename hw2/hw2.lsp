;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;QUESTION 1 OVERALL IMPLEMENTATION:
;If the first element is an atom/leaf, put at the front of the list and run the BFS function on the rest of the list
;If the first element is a list/subtree, expand the subtree and put its elements at the back of the list and run BFS on the whole new list
;If the list is nil, return nil

(defun BFS (FRINGE)
  (cond
   ((null FRINGE) nil)
   ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
   (t (BFS (append (cdr FRINGE) (car FRINGE))))
   ))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;

;QUESTION 2 OVERALL IMPLEMENTATION:
;FINAL-STATE:
    ;If the first element is nil, return nil
    ;Else run FINAL-STATE on the list minus the 1st element
    ;If list is nil, return t

;NEXT-STATE:
    ;If A==h, negate the 1st element
    ;If A==b, negate the 1st and 2nd element (If they're not equal, return nil)
    ;If A==d, negate the 1st and 3rd element (If they're not equal, or 2nd and 4th elements are, return nil)
    ;If A==p, negate the 1st and 4th element (If they're not equal, or 2nd and 3rd elements are, return nil)

;SUCC-FN:
    ;Append NEXT-STATE with S and each action, if it isn't nil, append it as (list (NEXT-STATE S A))

;ON-PATH:
    ;If the first element of PATH == S, return t, otherwise run ON-PATH on the list minus the first element

;MULT-DFS
    ;If STATE is empty, return nil
    ;Check if first element of STATE is FINAL-STATE. If so, return PATH + first element of STATE
    ;Check if first element of STATE is on PATH. If so, remove it from STATE and run MULT-DFS again
    ;Check if MULT-DFS on first element of STATE returns nil. If so, remove it from STATE and run MULT-DFS again
    ;If not, then return MULT-DFS on first element of STATE

;DFS
    ;If S is FINAL-STATE, return PATH+S
    ;Else return MULT-DFS with STATE=(SUCC-FS S), PATH=PATH+S

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
  (cond
   ((null S) t)
   ((null (car S)) nil)
   (t (FINAL-STATE (cdr S)))
   ))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  (cond
   ((equal A 'h) (cons (null (car S)) (cdr S)))
   ((equal A 'b)
    (cond
     ((equal (car S) (cadr S))
      (cons (null (car S)) (cons (null (car S)) (cddr S))))
     (t nil)
     ))
   ((equal A 'd)
    (cond
     ((and (equal (car S) (caddr S)) (null (equal (cadr S) (cadddr S))))
      (list (null (car S)) (cadr S) (null (car S)) (cadddr S)))
     (t nil)
     ))
   ((equal A 'p)
    (cond
     ((and (equal (car S) (cadddr S)) (null (equal (cadr S) (caddr S))))
      (list (null (car S)) (cadr S) (caddr S) (null (car S))))
     (t nil)
     ))
   ))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  (append
   (cond
    ((null (NEXT-STATE S 'h)) nil)
    (t (list (NEXT-STATE S 'h)))
    )
   (cond
    ((null (NEXT-STATE S 'b)) nil)
    (t (list (NEXT-STATE S 'b)))
    )
   (cond
    ((null (NEXT-STATE S 'd)) nil)
    (t (list (NEXT-STATE S 'd)))
    )
   (cond
    ((null (NEXT-STATE S 'p)) nil)
    (t (list (NEXT-STATE S 'p)))
    )
   ))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
  (cond
   ((null STATES) nil)
   ((equal S (car STATES)) t)
   (t (ON-PATH S (cdr STATES)))
   ))

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
  (cond
   ((null STATES) nil)
   ((FINAL-STATE (car STATES)) (append PATH (list (car STATES))))
   ((ON-PATH (car STATES) PATH) (MULT-DFS (cdr STATES) PATH))
   ((null (MULT-DFS (SUCC-FN (car STATES)) (append PATH (list (car STATES)))))
    (MULT-DFS (cdr STATES) PATH))
   (t (MULT-DFS (SUCC-FN (car STATES)) (append PATH (list (car STATES)))))
   ))

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
  (cond
   ((FINAL-STATE S) (list S))
   (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))
   ))
(print (DFS '(nil nil nil nil) 'nil))
