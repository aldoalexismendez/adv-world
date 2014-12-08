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


(define (thief? obj)
  (eq? (ask obj 'type) 'thief))

(define-class (place name)
  (parent (basic-object)) ;added to inherit basic-object
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (thieves '()) ;A6- refreshes on entry of new-person
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (method (type) 'place)
  (method (remove-from-place item) (set! things (remove item things))) ;helper method for B6
  (method (place?) #t) ;added for B4 part 2
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

   ; (set! thieves (filter thief? people))
  ;  (ask new-person 'look-around)
   
    (for-each (lambda (proc) (proc)) entry-procs)

    ;;;;;;;; A7 - make thieves notice when a person has entered
    ;;;;;;;; (note: this would also be the place to ask people to talk
    ;;;;;;;; when anyone *appears* in the place, regardless of whether the entering
    ;;;;;;;; person used the 'go' method or not. 
    ;;;;;;;; -KZ
   ; (let ((people-in-place (filter (lambda (thing) (eq? (ask thing 'type) 'thief)) people)))
;	  (set! thieves people-in-place)


    (for-each  (lambda (p) (ask p 'notice new-person)) people)
   (set! people (cons new-person people)))
	;	(filter (lambda (thing) (and (person? thing)
					  ;   (not (eq? thing new-person))))
		;	(ask p 'notice new-person)) ;don't run away from yourself
	       

   
    
    #| this sort of works but generates errors
    (for-each (lambda (p) (if (eq? (ask p 'type) 'thief)
			      (ask p 'notice new-person)
			      nil))
		     people)
    |#
    ;'appeared)))

  (method (gone thing)
    (if (not (memq thing things))
	(error "Disappearing thing not here" (list name thing)))
    (set! things (delete thing things)) 
    'disappeared)
  (method (add-thief obj)
    (set! thieves (cons obj thieves)))
  (method (exit person)
    (for-each (lambda (proc) (proc)) exit-procs)
    (if (not (memq person people))
	(error "Disappearing person not here" (list name person)))
   #| (if (eq? (ask person 'type) 'thief)
	(set! thieves (delete person thieves))
	nil)|#
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restaurant class for A7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|we're going to invent a new kind of place, called a restaurant. (That is,  restaurant is a subclass of place.) Each restaurant serves only one kind of food. (This is a simplification, of course, and it's easy to see how we might extend the project to allow lists of kinds of food.) When a restaurant is instantiated, it should have two extra arguments, besides the ones that all places have: the class of food objects that this restaurant sells, and the price of one item of this type:

   > (define-class (pasta) (parent (food ...)) ...)

   > (define somerestraurant (instantiate restaurant 'somerestaurant pasta 7))
Notice that the argument to the restaurant is a class, not a particular bagel (instance). Here is an example of the pasta food class. Your partner should have defined some example of food classes as part of B6.

>(define pesto-pasta (instantiate pasta))
>(ask pesto-pasta 'calories)
150
Restaurants should have two methods. The menu method returns a list containing the name and price of the food that the restaurant sells. The sell method takes two arguments, the person who wants to buy something and the name of the food that the person wants. The sell method must first check that the restaurant actually sells the right kind of food. If so, it should ask the buyer to pay-money in the appropriate amount. If that succeeds, the method should instantiate the food class and return the new food object. The method should return #f if the person can't buy the food.|#





       

(define-class (person name place)
  (parent (basic-object)) ;added to inherit basic-object
  (instance-vars
   (possessions '())
   (saying "")
   (testvar '())
   (my-ticket '()))
  (initialize
   (ask place 'enter self)
   (ask self 'put 'strength 70) ;added for B3
   (ask self 'put 'money 100)) ;added for A7
  (method (get-money amount) ;counterintuitively, this is supposed to *add* money
   (ask self 'add 'money amount))
  (method (pay-money amount)
      (if (> amount (ask self 'money))
	  (error: "Not enough money")
	  (eq? (ask self 'try-to-get-money amount) 'ok)))
  (method (try-to-get-money amount) ;helper method for pay-money in A7
     ;basically a codomain that gets mapped into a boolean range in pay-money 
       (ask self 'subtract 'money amount))
  (method (buy-food)
    (let ((amount-to-withdraw (cdr (ask place 'menu)))
	  (food-requested (car (ask place 'menu))))
      (if (eq? (ask self 'try-to-get-money amount-to-withdraw) 'ok)
	  (begin (set! possessions (cons food-requested possessions))
		 (ask self 'eat))
	  (error "I can't afford your delicious " food-requested ", " place ".  "))))
	  

(method (eat) ;B6
    (define (find-food lst)
      (define table (ask self 'table))
      (cond
        ((empty? lst) "All food as been consumed!")
        ((edible? (car lst)) 
          (begin
            (define food-item (car lst))
            (insert! 'strength (+ (lookup 'strength table) (ask food-item 'calories)) table)
            (ask place 'remove-from-place food-item)
            (set! possessions (remove food-item possessions))
            (find-food (cdr lst))
          )
        )
        (else (find-food (cdr lst)))))
    (find-food possessions)) ;B6

(method (has-laptop?) ;checks if person has laptop, B5
    (define (has-laptop-helper lst)
      (cond
        ((empty? lst) #f)
        ((ask (car lst) 'is-laptop?) #t)
        (else (has-laptop-helper (cdr lst)))))
    (has-laptop-helper possessions))
  (method (find-laptop)
    (define (find-laptop-helper lst)
      (cond
        ((empty? lst) "No laptop in possessions")
        ((ask (car lst) 'is-laptop?) (car lst))
        (else (find-laptop-helper (cdr lst)))))
    (find-laptop-helper possessions)) ;B5
  (define pc (ask self 'find-laptop)) ;Added, B5

  (method (person?) #t) ;added for B4 part 2


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

(method (take-all) ; B3, person takes all the items in a place (if it isnt already owned)
    (define items (ask place 'things))
    (define (check-if-owned items-lst new-items-lst) ;filters items to a list of unowned items
      (cond
        ((empty? items-lst) new-items-lst)
        ((equal? (ask (car items-lst) 'possessor) 'no-one) 
          (check-if-owned (cdr items-lst) (append new-items-lst (list (car items-lst))))) ;if item is owned by no one
        (else (check-if-owned (cdr items-lst) new-items-lst))))
    (define unowned-items (check-if-owned items '()))
    (define (take-all-helper lst)
      (cond
        ((empty? lst) "all items taken")
        (else 
          (begin
            (ask self 'take (car lst))
            (take-all-helper (cdr lst))))))
    (take-all-helper unowned-items)) ; copy up until here

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
  ;    (if  (null? (ask new-place 'exits))
     
      (if (not (eq? (ask place 'type) 'locked-place))
	  (begin
            (if (and (ask self 'has-laptop?) (ask place 'hotspot?)) ; copy from here: Automatically disconnects laptop from hotspot
              (begin
                (define computer (ask self 'find-laptop))
                (ask place 'remove-from-network computer)
                "laptop removed from network"
                )); up until here: automatically disconnects laptop from hotspot	     
	    
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))

	     
	      possessions)
	      (announce-move name place new-place)
	     (ask place 'exit self)
	     (set! place new-place)
	     
	     (ask new-place 'enter self))
	     
	     ;;;;;;;;;;;; A3 make people talk when someone enters
	   ;   (for-each
	  ;      (lambda (p)
	     ;;;;;;;;;;; A7 modifies A3 make thieves notice when a person has entered
	;	  (if (eq? (ask p 'type) 'thief)
	;	      (ask p 'strength)
;		      (ask p 'talk))
;		(ask place 'people))
	  
	     
	     
	      (error "Can't enter " (ask new-place 'name) ". It's locked!"))))
    (method (can-enter? place)
     (if (ask self 'has-exits? place)
	 (begin (if (eq? (ask place 'type) 'locked-place)
		    (ask place 'may-enter? (ask self 'name))
		    #t))
	 #f))
    (method (say-things)
	 (inventory self))
    (method (has-exits? place)
	(not (null? (ask place 'exits)))))


;;;;;;; A4 define locked-place

(define-class (locked-place name)
  (parent
   (place name))
  (instance-vars
   (locked #t))
  (method (unlock)
   (set! locked #t)
   'unlocked)
  (method (may-enter? person)
   (not (eq? locked #t)))

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
  (parent (basic-object)) ; inherits from basic-object

  (instance-vars
   (my-ticket-serial 0)
   (possessor 'no-one))
  (method (thing?) #t) ;added for B4 part 2

  (method (change-possessor new-possessor)
    (set! possessor new-possessor)
     'possessor)
  (method (type)
    'thing)
  (method (is-vehicle?) ;vehicle isn't a class, so use this predicate instead of type 
    (eq? (ask self 'name) 'vehicle))
  (method (current-ticket)
    (caar ticket-number)))





                                   
(define (name obj) (ask obj 'name))
#|
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

; copy from here









(define-class (basic-object) ; parent class of person, place and thing classes
  (instance-vars (table (make-table)))
  (method (put thing val) (insert! thing val table))
  (method (subtract thing val) ; assume the values are numeric
			      (let ((new-value (- (lookup thing table) val)))
				(insert! thing new-value table)))
  (method (add thing val)
    (let ((new-value (+ (lookup thing table) val)))
      (insert! thing new-value table)))
				
  (default-method (lookup message table))
)









(define-class (hotspot name pw) ;B5
  (instance-vars (laptop-list '()) (name-list '()))
  (parent (place name))
  (method (hotspot?) #t)
  (method (remove-from-network pc) (set! laptop-list (remove pc laptop-list)))
  (method (connect pc pword)
    (define owner (ask pc 'possessor)) 
    (cond
        ((and (equal? pword pw) (equal? self (ask owner 'place)))
          (begin
            (set! laptop-list (append laptop-list (list pc)))
            (set! name-list (append name-list (list (ask (last laptop-list) 'name))))
          ))
        ((equal? owner 'no-one) (error "laptop does not have an owner"))
        (else (error "password is wrong"))))
  (method (surf pc url) 
    (cond
      ((member? (ask pc 'name) name-list) (system (string-append "lynx " url)))
      (else (error "laptop is not connected")))))

(define-class (laptop name) ;B5, make sure laptop has owner + owner is in hotspot
  (parent (thing name))
  (method (is-laptop?) #t)
  (method (laptop-location owner) 
    (if (equal? owner 'no-one)
      (error "laptop has no owner")) 
    ask owner 'place)
  (method (connect pword)
    (define owner (ask self 'possessor))
    (define location (ask owner 'place))
    (cond
      ((ask location 'hotspot?) (ask location 'connect self pword))
      (else (error "location is not a hotspot"))))
  (method (surf url) 
    (define owner (ask self 'possessor))
    (define location (ask owner 'place))
    (ask location 'surf self url)
  )  
)

(define-class (food name calories) ;B6
  (class-vars
   (counter 0))
  (initialize
   (set! counter (+ counter 1)))
  (parent (thing name))
  (method (edible?) #t)
  (method (calories) calories)
  (method (type) 'food))

(define-class (pasta)
  (parent (food 'pasta 150)))

(define-class (salad)
  (parent (food 'salad 50)))

(define-class (steak)
  (parent (food 'steak 350)))

(define-class (soup)
  (parent (food 'soup 100)))

(define-class (burger)
  (parent (food 'burger 200)))  ; B6



(define pasta (instantiate food 'pasta 100))
(define salad (instantiate food 'salad 50))

(define-class (police name place)
  (parent (person name place))
  (initialize (ask self 'put 'strength 150))

  (method (take-thiefs-items items-lst)
    (define (take-thiefs-items-helper lst new-items-lst)
      (cond
        ((empty? lst) new-items-lst)
        (else (take-thiefs-items-helper (cdr lst) (append new-items-lst (car lst))))
      ))
    (take-thiefs-items-helper items-lst '())
  )

  (method (notice pers)
    (if (ask pers 'thief?)
      (begin
        (define thiefs-items (ask pers 'possessions))
        (set! possessions (append possessions thiefs-items))
        (ask pers 'remove-items)
        (display "Crime Does Not Pay")
        (ask thief 'go-directly-to-jail)
        ))
  )

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; some queue primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define-class (restaurant name food-class price)
  ;a restaurant is a type of object factory that creates food instances from a food class
  (parent
   (place name))
  (instance-vars
   (food-instance '())
   (foods '()))
  (initialize
   (ask self 'put (ask food-class 'name) price) ;menu
   (set! food-instance food-class)
   (ask self 'create-new-food-object)
  ; (ask moes 'push-new-food-object))
   )
  (method (create-new-food-object)
    (let ((new-food-instance ((lambda (x) (instantiate food x (ask x 'calories)))
			  food-instance)))
      (set! foods (cons new-food-instance foods))))
      ;(ask self 'appear new-food-instance)))
  (method (push-new-food-object)
    (if (null? food-instance)
	(ask self 'create-new-food-object) 
	(ask self 'appear food-instance)))
  (method (menu)
   (cadr (ask self 'table)))
  (method (sell pers obj)
    (let ((transaction (ask pers 'pay-money price)))
      (if (eq? transaction #t)
	  (ask pers 'take obj)
	  nil)))
      ; (ask person 'take obj)
       ;(error "I'm afraid you can't afford my delicious " food-class "."))
  (method (appear obj)
   (if (eq? (ask obj 'type) 'food)
       (insert! obj (ask obj 'name) foods)
       nil)
   (usual 'appear obj))))
  

(define owls (instantiate food 'owls 7))
(define moes (instantiate restaurant 'moes owls 10))

 (define a-girl-covered-in-feathers (instantiate person 'a-girl-covered-in-feathers moes))
 (ask a-girl-covered-in-feathers 'set-talk "Owls are low in calories, so you can eat a lot of owls still be ok") 
 (define a-long-faced-man (instantiate person 'a-long-faced-man moes))
 (ask a-long-faced-man 'set-talk "This is the worst date of my life")

(define kevin (instantiate person 'kevin moes))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

(define (edible? thing) (ask thing 'edible?))


(define-class (thief name place)
  (parent (person name place))
  ;(initialize
  ; (ask initial-place 'add-thief name))
  (instance-vars
   (behavior 'run))
  ;(initialize
   ;(ask self 'put 'strength 100)
   ;(ask self 'put 'behavior 'run))
  (method (type) 'thief)
  (method (thief?) #t)
  (method (remove-items) (set! (ask self 'possessions) '()))
  (method (change attribute new-attribute-value)
    (set! attribute new-attribute))

  (method (notice person)
    (if (eq? behavior 'run)
	; A6: I changed (ask (usual 'place) 'exits) to the code below
	; because the thief already defaults to its parent's place-asking method
	; -KZ
	;

	(let ((egress (pick-random (ask self 'exits))))
	  (if (null? egress)
	      nil
	    (begin (usual 'go egress)
	     (set! place (ask place 'look-in egress)))))))

		;(ask (ask (ask self 'place) 'look-in (pick-random egresses))
		 ;  'enter self)
	
	#|
	(let ((food-things
	       (filter (lambda (thing)
			 (and (edible? thing)
			      (not (eq? (ask thing 'possessor) self))))
		       (ask (usual 'place) 'things))))
	  (if (not (null? food-things))
	      (begin
	       (ask self 'take (car food-things))
	       (set! behavior 'run)
	       (ask self 'notice person)) ))))|#
  (method (go-directly-to-jail)
     (ask place 'exit self)
     (set! place superjail)
     (ask jail 'appear name)))
       

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
  (if (= (length set) 0)
      nil ; stop trying to leave the exitless place ya dummy
       (nth (random (length set)) set)))


(define (delete thing stuff)
  (cond ((null? stuff) '())
	((eq? thing (car stuff)) (cdr stuff))
	(else (cons (car stuff) (delete thing (cdr stuff)))) ))

(define (person? obj) ;implemented in B4 part 2
  (ask obj 'person?))

(define (thing? obj) ;implemented in B4 part 2
  (ask obj 'thing?))

(define (place? obj) ;implemented in B4 part 2
  (ask obj 'place?))
(define (locked? obj)
  (ask obj 'type))


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

;;; dummy places

(define jail (instantiate place 'jail))


(define beauty-pageant (instantiate place 'beauty-pageant))
(define burning-man (instantiate place 'burning-man))
(define sanitorium (instantiate place 'sanitorium))
(define nullspace (instantiate place 'nullspace))
(can-go nullspace 'north burning-man)
(can-go burning-man 'south nullspace)

(can-go nullspace 'south sanitorium)
(can-go sanitorium 'north nullspace)

(can-go nullspace 'east beauty-pageant)
(can-go beauty-pageant 'west nullspace)

(can-go moes 'into nullspace)
(can-go nullspace 'onto moes)



(define carlos-mencia (instantiate thief 'carlos-mencia nullspace))
(define nightmare-lions (instantiate person 'nightmare-lions jail))
(define diplo (instantiate person 'diplo burning-man))
(define children (instantiate person 'children beauty-pageant))


; some objects

(define tiaras (instantiate thing 'tiaras))
(ask beauty-pageant 'appear tiaras)


