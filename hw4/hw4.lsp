;
; CS161 Spring 2016: Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
  (+ (* (- n 1) k) c))

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
  (cond ((= c k) (list (node2var n c k)))
        (T (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))))

; Helper function to return single clause
; for each pair of colors, node is (not c1 OR not c2)
(defun createclause (n c1 c2 k)
  (list (- (node2var n c1 k)) (- (node2var n c2 k))))

; Helper function to return list of clauses for all possible pairs of colors starting from c1, to k
; (createclause c1 c1+1) (createclause c1 c1+2)... (createclause c1 k)
(defun notcolorpairs (n c1 c2 k)
  (cond ((> c2 k) nil)
        (T (append (list (createclause n c1 c2 k)) (notcolorpairs n c1 (+ c2 1) k)))))

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
; Explanation:
; (node n is color c1) => (n is not c2 AND n is not c3 AND ...n is not k)
; (n is not c1) OR (n is not c2 AND n is not c3 AND ...n is not k)
; ((n is not c1) OR (n is not c2)) AND ((n is not c1) OR (n is not c3)) AND ...((n is not c1) OR (n is not k))
;                                  ...AND ((n is not c3) OR (n is not c4)) AND ((n is not c3) OR (n is not c5))...
; Basically, for every possible pair of colors ci cj, need a clause saying: (n is not ci OR n is not cj)
(defun at-most-one-color (n c k)
  (cond ((= c k) nil)
        (T (append (notcolorpairs n c (+ c 1) k) (at-most-one-color n (+ c 1) k)))))

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
; Exactly one means at least one AND at most one
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k)))

; Helper function to iterate through colors from 1 to k
; specifying that the nodes in e cannot both be the same color
(defun diffcolors (e c k)
  (cond ((> c k) nil)
        (T (cons (list (- (node2var (car e) c k)) (- (node2var (cadr e) c k))) (diffcolors e (+ c 1) k)))))

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
; for each color c from 1 to k, not(e1 is c AND e2 is c)
; equivalent to (not e1 is c OR not e2 is c) for each clause in conjunction
(defun generate-edge-clauses (e k)
  (diffcolors e 1 k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun