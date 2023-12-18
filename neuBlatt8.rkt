#lang racket
;Aufgabe 1

(define (helper liste neueliste n)
  (cond ((and(null? (cdr liste))(= n 1)) (append neueliste (cons (car liste)'())))
        ((and(null? (cdr liste))(not(= n 1))) (append neueliste (cons n (cons(car liste)'()))))
        ((equal?(car liste)(cadr liste)) (helper (cdr liste) neueliste (+ n 1)))
        ((and(not(equal?(car liste)(cadr liste)))(not(= n 1))) (helper (cdr liste) (append neueliste (cons n (cons(car liste)'()))) 1))
        ((and(not(equal?(car liste)(cadr liste)))(= n 1)) (helper (cdr liste) (append neueliste (cons(car liste)'())) 1))))

(define (compress liste)
(helper liste '() 1))

;Aufgabe 2

(define (lel liste neueliste)
  (cond ((null? (cdr liste)) (append neueliste (cons(car liste)'())))
        ((and (integer? (car liste)) (=(car liste) 2)) (lel (cdr liste) (append neueliste (cons(cadr liste)'()))))
        ((integer? (car liste)) (lel (append(cons(-(car liste) 1)'()) (cdr liste)) (append neueliste (cons(cadr liste)'()))))
        ((not(integer? (car liste))) (lel (cdr liste) (append neueliste (cons(car liste)'()))))))

(define (expandiere sym-liste)
(lel sym-liste '()))

;Aufgabe 3
(define (loeschen liste n)
  (cond ((= 1 n) (cdr liste))
        ((< 1 n) (loeschen (cdr liste) (- n 1)))
        ((> 1 n) liste)))
          
