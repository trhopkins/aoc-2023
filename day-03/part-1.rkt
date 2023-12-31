#lang racket

(define lines
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(define matrix
  (for/vector #:length 140 ([line (in-list lines)])
    (for/vector ([char (in-list (string->list line))])
      char)))

(define num-posns
  (for/list ([line (in-list lines)]
             [i (in-naturals)])
    (cons i (regexp-match-positions* #rx"[0-9]+" line))))

(define (neighbors start end y)
  (for*/list ([x (in-range (sub1 start) (add1 end))]
              [y-offset (in-list (list -1 0 1))]
              #:unless (or (>= x 140)
                           (< x 0)
                           (>= (+ y y-offset) 140)
                           (< (+ y y-offset) 0)))
    (vector-ref (vector-ref matrix (+ y y-offset)) x)))

(define (symbol-adjacent? start end y)
  (ormap (λ (x) (regexp-match? #rx"[^0-9.]" (string x)))
         (neighbors start end y)))

(define part1
  (for/sum ([line-posns (in-list num-posns)]
            #:do ([define y (car line-posns)]
                  [define posns (cdr line-posns)]))
    (for/sum ([posn (in-list posns)]
              #:when (symbol-adjacent? (car posn) (cdr posn) y))
      (string->number (substring (list-ref lines y) (car posn) (cdr posn))))))

part1
