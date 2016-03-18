
;;
;; State format : (farmer fox goose grain)
;; With values of #f or #t
;;

;;
;; load search algorithms
;;

(load "search.ss")

;;
;; Test if state is valid or not
;; Fox and goose cannot be with each other unless with farmer
;;

(define farmer-is-valid-state?
  (lambda (state)
    (or
     (and
      (list-ref state 1)
      (list-ref state 2))
     (and
      (list-ref state 0)
      (list-ref state 1)
      )
     (and
      (list-ref state 0)
      (list-ref state 2)))))

;;
;; Test if State is a goal state
;;

(define farmer-goal?
  (lambda (state)
    (display "GOAL? ")
    (display state)
    (newline)
    (and
     (not (null? state))
     (equal? (car state) '(#t #t #t #t)))))

;;
;; Expand
;;

(define _hasMovedStates '(#t #f))

(define farmer-expand
  (lambda (state visited)
    (display "Expand ")
    (display state)
    (newline)
    (display visited)
    (newline)
    (farmer-remove-visited
      (filter farmer-is-valid-state? (farmer-move (car state) _hasMovedStates))
      visited)))

(define filter
  (lambda (pred ls)
    (cond
      ((null? ls) '())
      ((pred (car ls)) (filter pred (cdr ls)))
      (else (cons (car ls)
                  (filter pred (cdr ls)))))))

;;
;; Attempt to generate all reachable states
;;

(define farmer-move
  (lambda (state actor)
    (cond
      ((null? actor) '())
      (else (cons (list (car state) (car actor))
                        (cons (car actor) (cadr state)))
                  (farmer-move state (cdr actor))))))

;;
;; Remove states that were already visited
;;

(define farmer-remove-visited
  (lambda (states visited)
    (display "Remove")
    (display states)
    (display "   ")
    (display visited)
    (newline)
    (cond
      ((null? states) '())
      ((in-visited (car states) visited)
       (farmer-remove-visited (cdr states) visited))
      (else
       (cons (car states)
             (farmer-remove-visited (cdr states) visited))))))

;;
;; Test if state is in visited list
;;

(define in-visited
  (lambda (state visited)
    (and (not (null? visited))
         (if
          (not (equal? state (car visited)))
          (in-visited state (cdr visited))
          #t))))

;;
;; Print state
;;

(define farmer-print-state
  (lambda (state)
    (display "State ")
    (display (list-ref (car state) 0))
    (display (list-ref (car state) 1))
    (display (list-ref (car state) 2))
    (display (list-ref (car state) 3))
    (newline)))

;;
;; Search algorithms
;;

(define farmer-breadth-first
  (lambda (amount)
    (breadth-first-search (list amount '())
                          farmer-expand
                          farmer-goal?
                          farmer-print-state)))

(define fbf farmer-breadth-first)

(define farmer-depth-first
  (lambda (amount)
    (depth-first-search (list amount '())
                          farmer-expand
                          farmer-goal?
                          farmer-print-state)))

(define fdf farmer-depth-first)
