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
  (for*/fold ([acc 0])
             ([w (in-list (winners line))]
              [s (in-list (scratch line))]
              #:when (= w s))
    (add1 acc)))

(define instances (make-hash))

(for ([i (in-range (length lines))])
  (hash-set! instances i 1))

(hash-for-each
 instances
 (lambda (game count)
   (for ([i (in-range 1 (add1 (score (list-ref lines game))))])
     (hash-set! instances
                (+ game i)
                (+ (hash-ref instances (+ game i))
                   count)))))

(apply + (hash-map
          instances
          (lambda (game count)
            count)))