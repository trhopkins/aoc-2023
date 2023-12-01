#lang racket

; Advent of Code 2023 day 1 part 1

(define lines
  (call-with-input-file "01.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(define (numbers line)
  (regexp-match* #rx"[0-9]" line))

(define (first-last-num line)
  (define ns (numbers line))
  (string->number (string-append (first ns) (last ns))))

(for/sum ([l lines])
  (first-last-num l))
