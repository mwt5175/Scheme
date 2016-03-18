
;;
;; Part 2 Predicate calculus formulas
;;
;; For all x,y,z inst(x,y) ^ has-part(y,z) -> has-part(x,z)
;; For all x,y,z inst(x,y) ^ isa(y,z) -> isa(x,z)
;; For all x,y   inst(x,y) -> isa(x,y)
;;

;;
;; Semantic network design :
;;
;; The database structure stores the semantic network
;; in a list and uses recursion to iterate through all
;; instances.
;;
;; *database* => ((land-vehicle  (isa      (mode-of-transport)))
;;                (water-vehicle (isa      (mode-of-transport)))
;;                (air-vehicle   (isa      (mode-of-transport)))
;;                (car           (isa      (land-vehicle)))
;;                (car           (has-part (door)))
;;                ...
;;               )
;;

;;
;; Global database
;; - initially empty
;;

(define *database* '())

;;
;; Build a semantic net
;; - The relations are given in a file named file-name
;; - This procedure reads a line at a time as a list from file-name
;;   and builds the database using the procedure process-relation
;; - You need to complete the procedure process-relation below
;;   for this to work
;; - You don't need to modify this procedure

(define set-up-net
  (lambda (file-name)
    (with-input-from-file file-name
      (lambda ()
	(let loop ((in (read)))
	  (if (not (eof-object? in))
	      (begin
		(process-relation in)
		(loop (read)))))))))

;;
;; Updates *database*
;; - Currently, this procedure simply prints out its argument.
;; - You need to modify it so that it updates *database* using set!
;; - This is the only place you need to use set!
;;   So, do not abuse set! anywhere else.

(define process-relation
  (lambda (rel)
    (begin
      (set! *database* (append *database* (list (list (car (cdr rel)) (list (car rel) (cddr rel)))))))))

;;;
;;; Procedures that implement semantic net 
;;;

;;
;; Locates first instance of object and returns its parent
;; Used to locate instances since they are guaranteed to be unique
;;

(define (find-first-instance object NETWORK)
  (if (null? NETWORK)
      #f
      (if (equal? object (caar NETWORK))
	  (car (cadr (cadr (car NETWORK))))
	  (find-first-instance object (cdr NETWORK)))))

;;
;; Locates object property value pair in a network
;; given the parental relation and returns #t if it is found, #f otherwise.
;;

(define (lookup object property value parent-relation NETWORK)

  (define (get-parents object NET)
    (cond ((null? NET) '())
          ((and (equal? object (caar NET)) (equal? parent-relation (caadar NET)))
           (cons (car (cadr (cadar NET))) (get-parents object (cdr NET))))
          (else (get-parents object (cdr NET)))))
  
  (define (inherit parents)
    (if (null? parents) 
        #f
        (or (lookup (car parents) property value parent-relation NETWORK)
            (inherit (cdr parents)))))
    
  (if (member (list object (list property (list value))) NETWORK)
      #t
     (inherit (get-parents object NETWORK))))

;;
;; Test if x shares the 'isa' parental relationshup with y
;;

(define isa?
  (lambda (x y)
    (if (eq? x y)
	#t
	(lookup x 'isa y 'isa *database*))))

;;
;; modified to support instances
;;

(define has-part?
  (lambda (x y)
    (define (core)
      (let ((result (lookup x 'has-part y 'has-part *database*)))
	(if (not result)
	    (let ((inst (find-first-instance x *database*)))
	      (if inst
		  (has-part? inst y)
		  #f))
	    result)))
     (if (not (find-first-instance x *database*))
	#f
	(core))))

;;
;; Queries network. Returns list starting with object
;; of all parents that share the given relation
;;

(define query
  (lambda (object parent-relation NETWORK)

    (define (get-parents object NET)
    (cond ((null? NET) '())
         ((and (equal? object (caar NET)) (equal? parent-relation (caadar NET)))
          (cons  (car (cadr (cadar NET))) (get-parents object (cdr NET))))
          (else (get-parents object (cdr NET)))))

    (define (inherit parents)
      (if (not (null? parents))
        (or  (append (get-parents object NETWORK) (list (query (car parents) parent-relation NETWORK)))
             (inherit (cdr parents)))
        '()))

    (inherit (get-parents object NETWORK))))

;;
;; Yet another rewrite to better handle instances and objects
;; that have parent "isa" relationships and to produce correct
;; assignment output
;;

(define parts-of
  (lambda (object)

   (define (get-parents isa-list)
     (if (not (null? isa-list))
      (cons (query (car isa-list) 'has-part *database*)
      (get-parents (cdr isa-list)))
      '()))

    (define (recurse)
      (let ((isa-relationships (query object 'isa *database*)))
	(if (not (null? isa-relationships))
		(get-parents isa-relationships))))

    (define (core)
      (let ((values (query object 'isa *database*)))
	(if (not (null? values))
	    (let ((relationships (recurse)))
	      (if (not (null? relationships))
		  (append (query object 'has-part *database*) relationships)))
	    (parts-of (find-first-instance object *database*)))))

    (if (not (find-first-instance object *database*))
	#f
	(core))))
