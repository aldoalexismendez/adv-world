;; ADV.SCM
;; This file contains the definitions for the objects in the adventure
;; game and some utility procedures.

(define-class (place name)
  (parent (basic-object)) ;added to inherit basic-object
  (instance-vars
   (directions-and-neighbors '())
   (things '())
   (people '())
   (entry-procs '())
   (exit-procs '()))
  (method (remove-from-place item) (set! things (remove item things)))
  (method (type) 'place)
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
    'cleared) )

(define-class (person name place)
  (parent (basic-object)) ;added to inherit basic-object
  (instance-vars
   (possessions '())
   (saying ""))

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
  (initialize
   (ask place 'enter self)
  (ask self 'put 'strength 70)) ;added for B3
  (method (type) 'person)

  (method (person?) #t) ;added for B4 part 2

  (method (look-around)
    (map (lambda (obj) (ask obj 'name))
	 (filter (lambda (thing) (not (eq? thing self)))
		 (append (ask place 'things) (ask place 'people)))))
  (method (take thing)
    (cond ((not (thing? thing)) (error "Not a thing" thing))
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
      (cond ((null? new-place)
	     (error "Can't go" direction))
	    (else
	     (ask place 'exit self)
    (if (and (ask self 'has-laptop?) (ask place 'hotspot?)) ; copy from here: Automatically disconnects laptop from hotspot
      (begin
        (define computer (ask self 'find-laptop))
        (ask place 'remove-from-network computer)
        "laptop removed from network"
      )
    ); up until here: automatically disconnects laptop from hotspot
	     (announce-move name place new-place)
	     (for-each
	      (lambda (p)
		(ask place 'gone p)
		(ask new-place 'appear p))
	      possessions)
	     (set! place new-place)
	     (ask new-place 'enter self))))) )

(define-class (thing name)
  (parent (basic-object)) ; inherits from basic-object
  (instance-vars (possessor 'no-one))
  (method (name) name)
  (method (thing?) #t) ;added for B4 part 2
  (method (possessor) possessor)
  (method (type) 'thing)
  (method (change-possessor new-possessor) (set! possessor new-possessor))
)

; copy from here

(define-class (basic-object) ;parent class of person, place and thing classes
  (instance-vars (table (make-table)))
  (method (put thing val) (insert! thing val table))
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
  (parent (thing name))
  (method (edible?) #t)
  (method (calories) calories)
)

(define-class (pasta)
  (parent (food 'pasta 150))
)

(define-class (salad)
  (parent (food 'salad 50))
)

(define-class (steak)
  (parent (food 'steak 350))
)

(define-class (soup)
  (parent (food 'soup 100))
)

(define-class (burger)
  (parent (food 'burger 200))
)  ; B6

; up until here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation of thieves for part two
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *foods* '(pizza potstickers coffee))

(define (edible? thing) (ask thing 'edible?))

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

(define (person? obj) ;implemented in B4 part 2
  (ask obj 'person?))

(define (thing? obj) ;implemented in B4 part 2
  (ask obj 'thing?))

(define (place? obj) ;implemented in B4 part 2
  (ask obj 'place?))
