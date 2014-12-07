;;;  Data for adventure game.  This file is adv-world.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up the world
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Soda (instantiate place 'Soda))
(define BH-Office (instantiate place 'BH-Office))
(define MJC-Office (instantiate place 'MJC-Office))
(define art-gallery (instantiate place 'art-gallery))
(define Pimentel (instantiate place 'Pimentel))
(define 61A-Lab (instantiate place '61A-Lab))
(define Sproul-Plaza (instantiate place 'Sproul-Plaza))
(define Telegraph-Ave (instantiate place 'Telegraph-Ave))
(define Noahs (instantiate place 'Noahs))
(define Intermezzo (instantiate place 'Intermezzo))
(define Haas (instantiate place 'Haas-Business-School))
(define s-h (instantiate place 'sproul-hall))


(can-go Soda 'up art-gallery)
(can-go art-gallery 'down Soda)
(can-go art-gallery 'west BH-Office)
(can-go BH-Office 'east art-gallery)
(can-go art-gallery 'east MJC-Office)
(can-go MJC-office 'west art-gallery)
(can-go Soda 'down 61A-Lab)
(can-go 61A-Lab 'up Soda)
(can-go Soda 'south Pimentel)
(can-go Pimentel 'north Soda)
(can-go Pimentel 'south Haas)
(can-go Haas 'north Pimentel)
(can-go Haas 'west s-h)
(can-go s-h 'east Haas)
(can-go Sproul-Plaza 'east s-h)
(can-go s-h 'west Sproul-Plaza)
(can-go Sproul-Plaza 'north Pimentel)
(can-go Sproul-Plaza 'south Telegraph-Ave)
(can-go Telegraph-Ave 'north Sproul-Plaza)
(can-go Telegraph-Ave 'south Noahs)
(can-go Noahs 'north Telegraph-Ave)
(can-go Noahs 'south Intermezzo)
(can-go Intermezzo 'north Noahs)

;; Some people.
; MOVED above the add-entry-procedure stuff, to avoid the "The computers
; seem to be down" message that would occur when hacker enters 61a-lab
; -- Ryan Stejskal

(define Brian (instantiate person 'Brian BH-Office))
(define hacker (instantiate person 'hacker 61A-lab))
(define nasty (instantiate thief 'nasty sproul-plaza))

(define (sproul-hall-exit)
  (ask sproul-exit-counter 'increment)))
       
	   
;for A2 - stop erroring after 3rd attempt to leave sproul hall
(define-class (sproul-counter name)
  (instance-vars
   (times 0))

  (method (increment)
    (set! times (+ 1 times))
    (if (> times 3)
	(if (null? (ask s-h 'exit-procs))
	    nil
	    (ask s-h 'remove-exit-procedure sproul-exit-counter))
	    
	(error "You can check out any time you'd like, but you can never leave"))))
(define sproul-exit-counter (instantiate sproul-counter 'sproul-exit-counter))
  


(define (bh-office-exit)
  (print "What's your favorite programming language?")
  (let ((answer (read)))
    (if (eq? answer 'scheme)
	(print "Good answer, but my favorite is Logo!")
	(begin (newline) (bh-office-exit)))))


(ask s-h 'add-entry-procedure
 (lambda () (print "Miles and miles of students are waiting in line...")))
(ask s-h 'add-exit-procedure sproul-hall-exit)
(ask BH-Office 'add-exit-procedure bh-office-exit)
(ask Noahs 'add-entry-procedure
 (lambda () (print "Would you like lox with it?")))
(ask Noahs 'add-exit-procedure
 (lambda () (print "How about a cinnamon raisin bagel for dessert?")))
(ask Telegraph-Ave 'add-entry-procedure
 (lambda () (print "There are tie-dyed shirts as far as you can see...")))
(ask 61A-Lab 'add-entry-procedure
 (lambda () (print "The computers seem to be down")))
(ask 61A-Lab 'add-exit-procedure
 (lambda () (print "The workstations come back to life just in time.")))

;; Some things.

(define bagel (instantiate thing 'bagel))
(ask Noahs 'appear bagel)

(define coffee (instantiate thing 'coffee))
(ask Intermezzo 'appear coffee)

(define jolt (instantiate thing 'jolt))
(ask Soda 'appear jolt)


;; QUESTION 1: CREATE SELF, CREATE HOME, PUT SELF IN HOME



(define CZ (instantiate place 'CZ))
(define kevin (instantiate person 'kevin CZ))
(ask CZ 'appear kevin)

; Now connect CZ to campus


(can-go CZ 'east Soda)

; Create a place called Kirin north of Soda and put potstickers there

(define Kirin (instantiate place 'Kirin))
(can-go Soda 'north Kirin)
(can-go Kirin 'south Soda)
(define potstickers (instantiate thing 'potstickers))
(ask Kirin 'appear potstickers)

; Commands to move Kevin to Kirin, take potstickers, and leave them
; where Brian is:

(ask kevin 'go 'east)
;kevin moved from cz to soda
;appeared

(ask kevin 'go 'north)
;kevin moved from cz to kirin
;appeared

(ask kevin 'take potstickers)
;kevin took potstickers
;taken

(ask kevin 'go 'south)
;kevin moved from kirin to Soda
;appeared

(ask kevin 'go 'up)
(ask kevin 'go 'west)
(ask kevin 'lose potstickers)
(ask brian 'take potstickers)
(ask kevin 'go 'east)
(ask kevin 'go 'down)
(ask kevin 'go 'down)


; QUESTION 2

; 1. What kind of variable is brian?
; Brian is an object, which means that Brian is internally represented in Scheme as a procedure


; 2. What messages does a place understand?
; Places understand the "ask [placename] 'x" schema for the following x:
#|
type
(returns 'place for all places)
 neighbors
(returns list of adjacent place objects)
 exits
(returns a list of directions that are defined as passages from that places to its neighbors)
look-in '[direction]
(returns a place object if there is one; otherwise it returns the empty list)
appear thing
(adds a thing object to the local variable "things" (which is a list) iff. it does not exist
in this location and returns the message 'appeared. if it does exist it returns an error)
enter person
(adds a person object to the local variable "people" (which is a list) iff that person does not
exist in this location and returns the message 'appeared. if he/she does exist it returns an error)
gone thing
(deletes an object from the list of things that are present in the place and returns the message 'disappeared.
	 if object is not in current location it returns an error)
exit person
(deletes a person from the list of people present in the current location and returns the message 'disappeared.
	 if person is not in current location it returns an error)
new-neighbor direction neighbor
(defines a new passage from current place to another place in the direction specified and returns the message
  'connected. returns an error if that direction is already associated with a passage)
add-entry-procedure proc
(defines a procedure to be executed on entry of an object to the place)
add-exit-procedure proc
(defines a procedure to be executed on exit of an object from the place)
remove-entry-procedure proc
(removes a procedure from the list of procs executed on obj entry)
remove-exit-procedure proc
(removes a proc from the list of procs executed on exit)
clear-all procs
(removes all entry and exit procedures and returns 'cleared)
|#

; 3. (ask brian place) returns a place object. (ask (ask brian place) 'name) returns bh-office
; (let ((where (ask brian 'place))) (ask where 'name)) also returns bh-office.
; the reason is that the name is only accessible by querying the object, the name is not exposed to anyone but
; the object itself

#|
4. > (eq? (ask Telegraph-Ave 'look-in 'east) (ask Brian 'place))
#t

> (eq? (ask Brian 'place) 'Peoples-Park)
#f

> (eq? (ask (ask Brian 'place) 'name) 'Peoples-Park)
#t

first option is correct:
(ask 61a-lab 'appear computer)
This instantiates the *object* that Durer is to appear in the lab. #2 is incorrect because objects are called
by value, not by name. #3 is incorrect because it causes the message "durer" to appear in the lab separate
of any object.



