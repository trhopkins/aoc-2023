#lang racket

(define lines
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(define alphabet-map
  (hash #\2 #\2
        #\3 #\3
        #\4 #\4
        #\5 #\5
        #\6 #\6
        #\7 #\7
        #\8 #\8
        #\9 #\9
        #\T #\A
        #\J #\B
        #\Q #\C
        #\K #\D
        #\A #\E))

(define (alphabetize hand)
  (for/list ([char (in-string hand)])
    (hash-ref alphabet-map char)))

; element-wise xs < ys?
(define (char-list<? xs ys)
  (cond [(and (null? xs) (null? ys)) #f] ; tie breaker
        [(null? xs) #f]
        [(null? ys) #t]
        [(char<? (car xs) (car ys)) #t]
        [(char>? (car xs) (car ys)) #f]
        [else (char-list<? (cdr xs) (cdr ys))]))

(define (listnum<? l r)
  (cond #;[(and (null? l) (null? r)) #f]
        [(null? l) #f]
        [(null? r) #t]
        [(< (car l) (car r)) #t]
        [(> (car l) (car r)) #f]
        [else (listnum<? (cdr l) (cdr r))]))

(define hand-pairs
  (for/list ([line (in-list lines)])
    (define split (string-split line))
    (cons (car split) (string->number (cadr split)))))

(define elaborated-hands
  (for/list ([hp (in-list hand-pairs)]
             #:do ((define text (car hp))
                   (define alpha (alphabetize text))
                   (define count (sort (map length (group-by identity alpha)) >))
                   (define bid (cdr hp))))
    (list text alpha count bid)))

(define sorted-hands
    (sort (sort elaborated-hands char-list<? #:key cadr)
          listnum<?
          #:key caddr))

(for/sum ([hand (in-list sorted-hands)]
          [i (in-naturals)])
  (* (add1 i) (cadddr hand)))