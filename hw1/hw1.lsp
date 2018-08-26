;CS 161 - HW 1
;The solutions for question 1 and 2 are, unsurprisingly, very similar. Both check for the base cases of the padovan sequence (N = 0/1/2) and if N isn't a base case value, they then start 2 new recursive processes with arguments of N-2 and N-3
;The solution for question 3 returns nil if passed nil, returns ? if passed an atom, and otherwise recursively runs itself on the first element of the top-level list, and the rest of the list



;Question 1: (PAD N) - finds Nth element of padovian sequence (where PAD(0/1/2) = 1)
(defun PAD (N) (cond
		((< N 3) 1)
		(t (+ (PAD (- N 2)) (PAD (- N 3))))
		))


;Question 2: (SUMS N) - finds the number of sums used in the recursive calculation of PAD(N)
(defun SUMS (N) (cond
		 ((< N 3) 0)
		 (t (+ 1 (SUMS (- N 2)) (SUMS (- N 3))))
		 ))


;Question 3: (ANON TREE) - return anonomyzed version of TREE, with each atom turned into a ?
(defun ANON (TREE) (cond
		    ((null TREE) nil)
		    ((atom TREE) '?)
		    (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
		    ))
