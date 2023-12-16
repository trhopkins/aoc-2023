#lang racket

(define lines
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(define times (map string->number (cdr (string-split (car lines)))))
(define records (map string->number (cdr (string-split (cadr lines)))))

(for/product ([time (in-list times)]
          [record (in-list records)])
  (for/fold ([ways-to-win 0])
            ([t (in-range time)]
             #:do ((define distance (* t (- time t)))))
    (if (> distance record)
        (add1 ways-to-win)
        ways-to-win)))