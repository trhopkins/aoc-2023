#lang racket

(define lines
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(struct roll (id red green blue))

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

(define (game-power game)
  (define max-roll
    (for/fold ([acc (roll #f 0 0 0)])
              ([r (in-list game)])
      (roll #f
            (max (roll-red r) (roll-red acc))
            (max (roll-green r) (roll-green acc))
            (max (roll-blue r) (roll-blue acc)))))
  (* (roll-red max-roll) (roll-green max-roll) (roll-blue max-roll)))

(for/sum ([game all-games])
  (game-power game))
