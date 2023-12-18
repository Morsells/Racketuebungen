#lang racket
;lambda (x) (* x x)
;odd? Test ob die Zahl ungerade ist
;even? Gerade


(define (tuerme-von-hanoi n)
  (define (hanoi-helper n von nach hilfsstab)
    (cond ((= n 1)
           (list (cons von nach)))
          (else
           (append (hanoi-helper (- n 1) von hilfsstab nach)
                   (list (cons von nach))
                   (hanoi-helper (- n 1) hilfsstab nach von)))))
  
  (hanoi-helper n 'a 'c 'b))


(define (helper liste neueliste n)
  (cond ((and(null? (cdr liste))(= n 1)) (append neueliste (cons (car liste)'())))
        ((and(null? (cdr liste))(not(= n 1))) (append neueliste (cons n (cons(car liste)'()))))
        ((equal?(car liste)(cadr liste)) (helper (cdr liste) neueliste (+ n 1)))
        ((and(not(equal?(car liste)(cadr liste)))(not(= n 1))) (helper (cdr liste) (append neueliste (cons n (cons(car liste)'()))) 1))
        ((and(not(equal?(car liste)(cadr liste)))(= n 1)) (helper (cdr liste) (append neueliste (cons(car liste)'())) 1))))

(define (compress liste)
(helper liste '() 1))


;integer? Test ob Zahl ganzzahlig ist
;sqrt Quadratwurzel
;ceiling rundet auf
;Aufgabe 2
(define (helper liste aliste bliste n)
  (cond ((not(pair? liste)) (list aliste bliste))
        ((and(null? (cdr liste)) (odd? n)) (list (append aliste (cons (car liste) '())) bliste))
        ((null? (cdr liste)) (list aliste(append bliste (cons (car liste) '()))))
        ((odd? n) (helper (cdr liste) (append aliste (cons (car liste) '()))bliste (+ n 1)))
        ((not(odd? n)) (helper (cdr liste) aliste (append bliste (cons (car liste) '())) (+ n 1)))))

(define (liste-teilen eingabe)
(helper eingabe '() '() 1))

(define (einfuegen x liste)
(cond ((null? liste) (list x))
((< x (car liste)) (cons x liste))
(else (cons (car liste) (einfuegen x (cdr liste))))))


;Wechsel von Groß-/Kleinschreibung
;(char-upcase char) (char-downcase char)
;Umwandlung zwischen char und integer
;(char->integer char) (integer->char k)

;(quotient 32 5) 6
;(remainder 32 5) 2


;Aufgabe 3
(define (lol liste1 liste2 neueliste n)
  (cond ((and(null? (cdr liste1)) (null?(cdr liste2))(odd? n)) (append neueliste (cons (car liste1)(cons (car liste2)'()))))
        ((and(null? (cdr liste1)) (null?(cdr liste2))(not(odd? n))) (append neueliste (cons (car liste2)(cons (car liste1)'()))))
        ((null? (cdr liste1)) (lol liste1 (cdr liste2) (append neueliste (cons (car liste2) '())) (+ n 1)))
        ((null? (cdr liste2)) (lol (cdr liste1) liste2 (append neueliste (cons (car liste1) '())) (+ n 1)))       
        ((odd? n) (lol (cdr liste1) liste2 (append neueliste (cons (car liste1) '())) (+ n 1)))
        ((not(odd? n)) (lol liste1 (cdr liste2) (append neueliste (cons (car liste2) '())) (+ n 1)))))

(define (listen-verschmelzen eingabe)
(let ((liste1 (car eingabe))
     (liste2 (cadr eingabe)))
  (lol liste1 liste2 '() 1)))
;(listen-verschmelzen '((1 3 5 7 9) (2 4 6 8))) ;(string->list "Hello world") (#\H #\e #\l #\l #\o #\space #\w #\o #\r #\l #\d)

;Aufgabe 4
;(pair? paar)
;equal? Test ob die werte identisch sind

(define (sel liste1 liste2 i)
  (cond ((and(null? (cdr liste1)) (= (car liste1) (car liste2))) i )
        ((and(null? (cdr liste1)) (not(= (car liste1) (car liste2))))  (+ i 1) )
        ((= (car liste1) (car liste2)) (sel (cdr liste1) (cdr liste2) i))
        ((not(= (car liste1) (car liste2))) (sel (cdr liste1) (cdr liste2) (+ i 1) ))))

(define (hamming liste1 liste2)
  (sel liste1 liste2 0))


(hamming '(1 0 1) '(1 0 1)) 

(hamming '(1 0 1 1 0 1 0 1) '(0 1 1 1 0 1 0 0))

;Aufagbe 2
(define (drehe liste)
(define (helper liste neuliste)
  (cond ((null? (cdr liste)) (cons (car liste) neuliste))
        ((helper (cdr liste) (cons (car liste) neuliste)))))
 (helper liste '()))

; (symbol? 'apfel) #t
;append hängt listen einander
;char? test ob es ein string ist
;Ordnung auf Zeichen char<?, char>?, char<=?, char>=?, char=?
;Ordnung auf Zeichen ohne Berücksichtigung von Groß-/Kleinschreibung char-ci<?, char-ci>?, char-ci<=?,char-ci>=?, char-ci=?
