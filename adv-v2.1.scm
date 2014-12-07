;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(load "tables.scm")

(define-class (ticket number)
  (parent
   (thing number))
  (instance-vars
   (serial 0)
   (ticket-owner '()))
  (initialize
   (set! serial number))
  (method (type) 'ticket)
  (method (name-on-ticket)
     (ask self 'possessor)))


(define-class (place name)
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (method (type) 'place)
  (method (neighbors) (map cdr directions-and-neighbors))
  (method (exits) (map car directions-and-neighbors))
  (method (look-in direction)
    (let ((pair (assoc direction directions-and-neighbors)))
      (if (not pair)
	  '()                     ;; nothing in that direction
	  (cdr pair))))           ;; return the place object
  (method (appear new-thing)
    (if (memq new-thing things)
	(error "Thing already in this place" (list name new-thing)))
    (set! things (cons new-thing things))

    'appeared)
  (method (enter new-person)
    (if (memq new-person people)
	(error "Person already in this place" (list name new-person)))
    (set! people (cons new-person people))
    (for-each (lambda (proc) (proc)) entry-procs)
    'appeared)
  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
    (set! people (delete person people)) 
    'disappeared)
  (method (new-neighbor direction neighbor)
    (if (assoc direction directions-and-neighbors)
	(error "Direction already assigned a neighbor" (list name direction)))
    (set! directions-and-neighbors
	  (cons (cons direction neighbor) directions-and-neighbors))
    'connected)
  (method (add-entry-procedure proc)
    (set! entry-procs (cons proc entry-procs)))
  (method (add-exit-procedure proc)
    (set! exit-procs (cons proc exit-procs)))
  (method (remove-entry-procedure proc)
    (set! entry-procs (delete proc entry-procs)))
  (method (remove-exit-procedure proc)
    (set! exit-procs (delete proc exit-procs)))
  (method (clear-all-procs)
    (set! exit-procs '())
    (set! entry-procs '())
    'cleared))


       

(define-class (person name place)
  (instance-vars
   (possessions '())
   (saying "")
   (testvar '())
   (my-ticket '()))
  (initialize
   (ask place 'enter self))
  (method (type) 'person)
  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
    (cond ((and (not (thing? thing))
		(not (ticket? thing))) ; KZ modification for A5
	         (error "Not a thing" thing))

	   ; note to aldo:
	   ; Tickets are a child class of thing, so there's some ambiguity
	   ; whether
	   ;          (thing? example-ticket-obj)
	   ;
	   ; should return #f or #t. I will have it return false for now
	   ; and if it makes more sense to change it later, we can do that
	   ;

	  ((not (memq thing (ask place 'things)))
	   (error "Thing taken not at this place"
		  (list (ask place 'name) thing)))
	  ((memq thing possessions) (error "You already have it!"))
	  (else
	   (announce-take name thing)
	   (set! possessions (cons thing possessions))
	       
	   ;; If somebody already has this object...
	   (for-each
	    (lambda (pers)
	      (if (and (not (eq? pers self)) ; ignore myself
		       (memq thing (ask pers 'possessions)))
		  (begin
		   (ask pers 'lose thing)
		   (have-fit pers))))
	    (ask place 'people))
	       
	   (ask thing 'change-possessor self)

	   ; if person is taking a ticket, then cache it in a variable
	   ; called my-ticket so we don't have to search for it later
	   
	   (if (ticket? thing)
	       (set! my-ticket thing)
	       nil)



	   
	   'taken)))

  (method (lose thing)
    (set! possessions (delete thing possessions))
    (ask thing 'change-possessor 'no-one)
    'lost)
  (method (talk) (print saying))
  (method (set-talk string) (set! saying string))
  (method (exits) (ask place 'exits))
  (method (notice person) (ask self 'talk))
  (method (go direction)
    (let ((new-place (ask place 'look-in direction)))
      (if (ask self 'can-enter? new-place)
	  (begin
      (cond ((null? new-place)
	     (error "Can't go" direction))
	    (else
#|
	     ;if you're leaving sproul...
	     (if (eq? (ask (ask self 'place) 'name)
		      (ask s-h 'name))
		 ;increment the counter...
		 (set! (sproul-hall-exit-counter (+ sproul-hall-exit-counter 1)))
		 ;otherwise do nothing and also the stuff below
		 nil)
		 |#
	     (ask place 'exit self)
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))

	     
	      possessions)
	     (set! place new-place)

	     ;;;;;;;;;;;; A3 make people talk when someone enters
	      (for-each
	        (lambda (p)
		  (ask p 'talk))
		(ask place 'people))
	  
	       
	     
	     (ask new-place 'enter self))))
	  (error "Can't enter " place". It's locked!"))))
    (method (can-enter? place)
    (if (eq? (ask place 'type) 'locked-place)
	(ask place 'may-enter?)
	#t))
    (method (say-things)
	 (inventory self)))


;;;;;;; A4 define locked-place

(define-class (locked-place name)
  (parent
   (place name))
  (instance-vars
   (may-enter? #f))
  (method (unlock)
   (set! may-enter? #t)
   'unlocked)
  (method (type) 'locked-place))


(define-class (garage name)
  (parent
   (place name))
  (class-vars
   (ticket-numbers '())) ; basically a counter. shared between garages so that each
                         ; ticket serial is unique
  (instance-vars
   (tickets-issued '())
   (inventory '())
   (cars '())
   (current-ticket-serial 0) ; auxiliary vars for ticket counter
   (next-ticket-serial 0))
  (initialize
   (set! cars (make-table))
   (set! inventory (make-table))
   ; first garage initializes the ticket counter
   (if (null? ticket-numbers)
       (set! ticket-numbers (cons 0 ticket-numbers))
       nil)
	 
   ; thread the current and next values	   
   (set! current-ticket-serial (car ticket-numbers))
   (set! next-ticket-serial (+ 1 (car ticket-numbers))))
  (method (appear obj)
   (if (eq? (ask obj 'name) 'vehicle)
       (insert! obj (ask obj 'possessor) cars)
       nil)
   (usual 'appear obj))
       
  

  (method (destruct-pointer! obj)
	  ; A5 - helper method for unpark. destroys the key/value pair in the inventory
	  ; and returns #f afterward for the key
	  (cond ((ticket? obj) (let ((the-vehicle (lookup (ask obj 'serial)
					     (ask self 'inventory))))
				     
				 (set! the-vehicle '#f)))))
  (method (park vehicle)
   (cond ((memq vehicle cars)
	  (error "You gotta bring it here, man"))
	 ((eq? (ask vehicle 'possessor) 'no-one)
	  (error "I don't know who owns that car."))
	 (else
	  (let ((new-ticket (instantiate ticket next-ticket-serial))
		(vehicle-owner (ask vehicle 'possessor)))
       ; remember that tickets don't have a unique name, they are ID'd by serial e.g.:
       ;          STk> (ask new-ticket 'serial)
	        (insert! (ask new-ticket 'serial) vehicle inventory)
		(set! tickets-issued (cons (ask new-ticket 'serial) tickets-issued))
		(ask self 'appear new-ticket)
		(ask vehicle-owner 'take new-ticket)
		(ask new-ticket 'change-possessor vehicle-owner)
		(ask vehicle-owner 'lose vehicle)
		(ask self 'tear-off-ticket) 
		))))
  (method (unpark obj)
	  (cond ((and (ticket? obj)
		      (not (eq? (lookup (ask obj 'serial) (ask self 'inventory)) #f)))
		 (let ((vehicle-owner (ask obj 'possessor))
		       (the-vehicle (lookup (ask obj 'serial) (ask self 'inventory))))
		   (ask vehicle-owner 'take the-vehicle)
		   (ask self 'destruct-pointer! obj)))
		((eq? (ask obj 'possessor) #f) ;taken cars return #f
		 (error "Think you already took that car."))
		(else (error "That's a " (ask obj 'name) "."))))
			 
			 
  (method (tear-off-ticket)  ;updates current/next serial values
   (set! current-ticket-serial (car tickets-issued))
   (set! next-ticket-serial (+ 1 (car tickets-issued)))))







#|

(define thing
  (let ()
    (lambda (class-message)
      (cond
       ((eq? class-message 'instantiate)
	(lambda (name)
	  (let ((self '()) (possessor 'no-one))
	    (define (dispatch message)
	      (cond
	       ((eq? message 'initialize)
		(lambda (value-for-self)
		  (set! self value-for-self)))
	       ((eq? message 'send-usual-to-parent)
		(error "Can't use USUAL without a parent." 'thing))
	       ((eq? message 'name) (lambda () name))
	       ((eq? message 'possessor) (lambda () possessor))
	       ((eq? message 'type) (lambda () 'thing))
	       ((eq? message 'change-possessor)
		(lambda (new-possessor)
		  (set! possessor new-possessor)))
	       (else (no-method 'thing))))
	    dispatch)))
       (else (error "Bad message to class" class-message))))))
|#

; define thing class using new object notation

(define-class (thing name)
  (instance-vars
   (my-ticket-serial 0)
   (possessor 'no-one))
  (method (change-possessor new-possessor)
    (set! possessor new-possessor)
     'possessor)
  (method (type)
    'thing)
  (method (is-vehicle?) ;vehicle isn't a class, so use this predicate instead of type 
    (eq? (ask self 'name) 'vehicle))
  (method (current-ticket)
    (caar ticket-number)))


#|


                                   
(define (name obj) (ask obj 'name)) leave this commented out for now,
                                    i think it's causing bugs with park/unpark

(define (inventory obj)
  (if (person? obj)
      (map name (ask obj 'possessions))
      (map name (ask obj 'things))))
|#
; write a procedure WHEREIS that takes a person and returns
; the name of the place where that person is
(define (whereis obj)
  (if (person? obj)
      (ask (ask obj 'place) 'name)
      (error "Argument to whereis must be a person" )))

(define (owner obj)
  (if (thing? obj)
      (if (person? (ask obj 'possessor))
	  (ask (ask obj 'possessor) 'name)
	  (ask obj 'possessor))
      (error "Argument to owner must be a person")))

(define (ticket? obj)
  (eq? (ask obj 'type) 'ticket))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

(define (edible? thing)
  (member? (ask thing 'name) *foods*))

(define-class (thief name initial-place)
  (parent (person name initial-place))
  (instance-vars
   (behavior 'steal))
  (method (type) 'thief)

  (method (notice person)
    (if (eq? behavior 'run)
	(ask self 'go (pick-random (ask (usual 'place) 'exits)))
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) )))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; this next procedure is useful for moving around

(define (move-loop who)
  (newline)
  (print (ask who 'exits))
  (display "?  > ")
  (let ((dir (read)))
    (if (equal? dir 'stop)
	(newline)
	(begin (print (ask who 'go dir))
	       (move-loop who)))))


;; One-way paths connect individual places.

(define (can-go from direction to)
  (ask from 'new-neighbor direction to))


(define (announce-take name thing)
  (newline)
  (display name)
  (display " took ")
  (display (ask thing 'name))
  (newline))

(define (announce-move name old-place new-place)
  (newline)
  (newline)
  (display name)
  (display " moved from ")
  (display (ask old-place 'name))
  (display " to ")
  (display (ask new-place 'name))
  (newline))


(define (have-fit p)
  (newline)
  (display "Yaaah! ")
  (display (ask p 'name))
  (display " is upset!")
  (newline))


(define (pick-random set)
  (nth (random (length set)) set))

  

(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj)
  (and (procedure? obj)
       (member? (ask obj 'type) '(person police thief))))

(define (thing? obj)
  (and (procedure? obj)
       (eq? (ask obj 'type) 'thing)))

(define noahs-park (instantiate garage 'noahs-park))
(define the-pope (instantiate person 'the-pope noahs-park))
(define popemobile (instantiate thing 'vehicle))
(ask noahs-park 'appear popemobile)
(ask the-pope 'take popemobile)
(ask noahs-park 'park popemobile)
(display "Woohoo my ticket's serial number is ")
(display (ask (ask the-pope 'my-ticket) 'serial))
(display (newline))
(newline)
(display "I want my car back ")
(ask noahs-park 'unpark (ask the-pope 'my-ticket))

#|STk> 
the-pope took vehicle
okay
STk> (ask noahs-park 'park popemobile)
ok
STk> (ask noahs-park 'inventory)
(*table* (1 . #[closure arglist=(message) 43d5cc]))
STk> (lookup 1 (noahs-park 'inventory))
*** Error:
    cdr: wrong type of argument: #[closure arglist=() 2b727c]
Current eval stack:
__________________
  0    (cdr table)
  1    (assoc key (cdr table))
  2    (let ((record (assoc key (cdr table)))) (if (not record) #f (cdr record)))
STk> (lookup 1 (ask noahs-park 'inventory))
#[closure arglist=(message) 43d5cc]
STk> (ask (lookup 1 (ask noahs-park 'inventory)) 'name)
vehicle
STk> (ask (lookup 1 (ask noahs-park 'inventory)) 'possessor)
#[closure arglist=(message) 4407b8]
STk> (ask (ask (lookup 1 (ask noahs-park 'inventory)) 'possessor) 'name)
the-pope
|#

