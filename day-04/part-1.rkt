#lang racket

(require threading) ; thank you Alexis King!

(define lines
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(define (winners line)
  (~>> line
       (string-split _ "|")
       car
       (string-split _ ":")
       cadr
       string-split
       (map string->number)))

(define (scratch line)
  (~>> line
       (string-split _ "|")
       cadr
       string-split
       (map string->number)))

(define (score line)
  (floor (/ (for*/product ([w (in-list (winners line))]
                           [s (in-list (scratch line))]
                           #:when (= w s))
              2)
            2)))

(define total
  (for/sum ([line lines])
    (score line)))

total