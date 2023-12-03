#lang racket

(define lines
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(struct roll (id red green blue))

(define reds-max 12)
(define greens-max 13)
(define blues-max 14)

(define all-games
  (for/list ([line (in-list lines)])
    (match-define (list id trials-text) (string-split (string-trim line "Game ") ": "))
    (for/list ([trial (in-list (string-split trials-text "; "))])
      (for/fold ([acc (roll (string->number id) 0 0 0)])
                ([color (in-list (string-split trial ", "))])
        (match (string-split color)
          [(list (app string->number n) "red") (struct-copy roll acc (red n))]
          [(list (app string->number n) "green") (struct-copy roll acc (green n))]
          [(list (app string->number n) "blue") (struct-copy roll acc (blue n))])))))

(define valid-games
  (for/sum ([game (in-list all-games)]
             #:when (andmap (lambda (g)
                              (and (<= (roll-red g) reds-max)
                                   (<= (roll-green g) greens-max)
                                   (<= (roll-blue g) blues-max)))
                            game))
    (roll-id (first game))))

valid-games
