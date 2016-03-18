
;; Input:
;;   merge-queue
;;     -- algorithm specific
;;     -- procedure that takes a list of new paths and a queue
;;        and returns a new queue
;;   init-state
;;     -- problem-specific
;;     -- an initial state to start the search from
;;   expand
;;     -- problem-specific
;;     -- procedure that takes a state and returns a list of
;;        states that are reachable in one move from the given state
;;   goal?
;;     -- problem-specific
;;     -- predicate that takes a state and returns true if the
;;        state is a goal state, false otherwise
;;   print-state
;;     -- problem-specific
;;     -- procedure that takes a state and prints out a state
;;        nicely
;;
;; Output:
;;   -- When succeeded, a path from the initial state to a goal state
;;   -- When failed, #f

(define search
  (lambda (merge-queue)
    (lambda (init-state expand goal? print-state)
    (letrec
     ((search-helper
       (lambda (queue visited)
	 (display "\nCurrent Queue\n")
	 (print-queue print-state queue)
	 (cond
	  ((null? queue)
	   (display "No Goal State Found\n")
	   #f)
	  ((goal? (caar queue))
	   (display "Goal State Reached\n")
	   (print-state (caar queue))
	   (newline)
	   (let ((ans (reverse (car queue))))
	     (display "Path to Goal State:\n")
	     (for-each (lambda (s) (print-state s) (newline))
		       ans)
	     (newline)
	     ans))
	  (else
	   (let ((successors (expand (caar queue) visited)))
	     (print-state (caar queue))
	     (cond
	      ((null? successors)
	       (display "No Successor State Found\n")
	       (search-helper (cdr queue) visited))
	      (else
	       (search-helper
		(merge-queue (expand-path successors (car queue)) (cdr queue))
		(append successors visited))))))))))
     (search-helper
      (list (list init-state))
      (list init-state))))))

(define expand-path
  (lambda (successors path)
    (cond
     ((null? successors) '())
     (else (cons (cons (car successors) path)
		 (expand-path (cdr successors) path))))))

(define print-queue
  (lambda (print-state queue)
    (display "Contains ")
    (display (length queue))
    (display " Paths\n")
    (newline)))

(define depth-first-merge
  (lambda (path queue)
    (append path queue)))

(define breadth-first-merge
  (lambda (path queue)
    (append queue path)))

(define depth-first-search (search depth-first-merge))

(define breadth-first-search (search breadth-first-merge))
