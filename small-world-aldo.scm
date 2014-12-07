;;; small-world.scm
;;; Miniature game world for debugging the CS61A adventure game project.
;;; You can load this instead of adv-world.scm, and reload it quickly
;;; whenever you change a class.

;;; How to use this file:
;;; If, for example, your person class doesn't work, and you do something
;;; like (define Matt (instantiate person 'Matt)), and then fix your
;;; person class definition, Matt is still bound to the faulty person
;;; object from before.  However, reloading this file whenever you
;;; change something should redefine everything in your world with the
;;; currently loaded (i.e. most recent) versions of your classes.

(define 61A-Lab (instantiate place '61A-Lab))
(define Lounge (instantiate place 'Lounge))
(can-go 61A-Lab 'up Lounge)
(can-go Lounge 'down 61A-Lab)
;;;  Hopefully you'll see more of the world than this in real life
;;;  while you're doing the project!

(define homework-box (instantiate thing 'homework-box))
(ask 61A-Lab 'appear homework-box)

(define Coke (instantiate thing 'Coke))
(ask Lounge 'appear Coke)

(define laba (instantiate person 'Lab-assistant 61A-Lab))
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
(define library (instantiate hotspot 'library '1234))
(define aldo (instantiate person 'aldo library))
(define clone-aldo (instantiate person 'clone-aldo library))
(define pc1 (instantiate laptop 'pc1))
(define pc2 (instantiate laptop 'pc2))
(ask library 'appear pc1)
(ask library 'appear pc2)
(ask aldo 'take pc1)
(ask clone-aldo 'take pc2)
(define pc3 (instantiate laptop 'pc3))
(define aldo3 (instantiate person 'aldo3 library))
(ask library 'appear pc3)
(ask library 'connect pc1 '1234)
(ask pc2 'connect '1234)
(define candy (instantiate thing 'candy))
(ask library 'appear candy)
(define jolt (instantiate thing 'jolt))
(ask library 'appear jolt)






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
(can-go library 'west Soda)
