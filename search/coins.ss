
(load "search.ss")

;;
;; there are 7 kinds of old British coins
;;

(define old-british-coins '(120 30 24 12 6 3 1))

;;
;; or, you can do the same for US coins
;;

(define us-coins '(100 50 25 10 5 1))

;;
;; here, we will do the British coins
;;

(define *coins* old-british-coins)

;;
;; is the state the goal state?
;;

(define coin-goal?
  (lambda (state)
    (zero? (car state))))

;;
;; find all the legal states reachable from the given state
;; in one move.
;;

(define coin-expand
  (lambda (state visited)
    (coin-remove-visited
      (filter coin-bad-state? (coin-move state *coins*))
      visited)))

;;
;; is a state illegal?
;;

(define coin-bad-state?
  (lambda (state)
    ;; amount of change can't be less than 0
    (< (car state) 0)))

;;
;; generate all the states resulting from applying each coin
;; to the given state
;;

(define coin-move
  (lambda (state coins)
    (cond
      ((null? coins) '())
      (else (cons (list (- (car state) (car coins))
                        (cons (car coins) (cadr state)))
                  (coin-move state (cdr coins)))))))

;;
;; remove already visited state from the list of states
;;

(define coin-remove-visited
  (lambda (states visited)
    (cond
      ((null? states) '())
      ((any? (lambda (x) (= (caar states) (car x))) visited)
       (coin-remove-visited (cdr states) visited))
      (else
       (cons (car states)
             (coin-remove-visited (cdr states) visited))))))

(define any?
  (lambda (pred ls)
    (and (not (null? ls))
	 (or (pred (car ls))
	     (any? pred (cdr ls))))))

;;
;; filter out those in ls that returns #t to pred
;;

(define filter
  (lambda (pred ls)
    (cond
      ((null? ls) '())
      ((pred (car ls)) (filter pred (cdr ls)))
      (else (cons (car ls)
                  (filter pred (cdr ls)))))))

;;
;; Prints a state
;;

(define coin-print-state
  (lambda (x)
    (let ((change (car x))
	  (paid (if (null? (cadr x)) '(0) (cadr x))))
      (display "PAID: ")
      (display (cons '+ paid))
      (display "    ")
      (display "REMAINDER: ")
      (display change)
      (newline))))

;;
;; depth-first-search
;;

(define coin-depth-first
  (lambda (amount)
    (depth-first-search (list amount '())
                        coin-expand
                        coin-goal?
                        coin-print-state)))

;;
;; breadth-first-search
;;

(define coin-breadth-first
  (lambda (amount)
    (breadth-first-search (list amount '())
                          coin-expand
                          coin-goal?
                          coin-print-state)))
