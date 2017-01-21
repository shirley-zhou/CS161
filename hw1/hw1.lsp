;;; Question 1:
;; argument: some integer N, returns: the nth Padovan number, where P(N) = P(N-2) + P(N-3)
(defun PAD(N)
  (cond ((= N 0) 1) ; base cases P(0) = P(1) = P(2) = 1, hardcoded
        ((= N 1) 1)
        ((= N 2) 1)
        (T (+ (PAD (- N 2)) (PAD (- N 3)))))) ; implement formula for padovan numbers

;;; Question 2:
;; argument: some integer N, returns the number of additions required in PAD(N)
(defun SUMS(N)
  (cond ((= N 0) 0) ; base cases are hardcoded, require no addition
       ((= N 1) 0)
       ((= N 2) 0)
       (T (+ 1 (SUMS (- N 2)) (SUMS (- N 3))))))
       ; total sums = number of sums for PAD(- N 2) + sums for PAD(- N 3) + current sum for PAD(N)

;;; Question 3:
;; argument: a tree given as a list, returns the anonymized version of the tree
;; retaining structure but replacing values by '?'
(defun ANON(TREE)
  (cond ((null TREE) nil) ; base case: empty tree, nil
        ((atom TREE) '?) ; base case: leaf -> change to ?
        (T (cons (ANON (car TREE)) (ANON (cdr TREE))))))
        ; construct the new tree out of anonymized first branch (car) attached to anonymized rest
