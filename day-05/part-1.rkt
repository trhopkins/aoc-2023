#lang racket

(define lines
  (call-with-input-file "input.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

; refusing to clean this up
(define seeds
  (map string->number (string-split (string-trim (car lines) "seeds: "))))
(define-values (seeds-lines rest-0)
  (splitf-at lines non-empty-string?))
(define-values (s2s-lines rest-1)
  (splitf-at (cddr rest-0) non-empty-string?))
(define-values (s2f-lines rest-2)
  (splitf-at (cddr rest-1) non-empty-string?))
(define-values (f2w-lines rest-3)
  (splitf-at (cddr rest-2) non-empty-string?))
(define-values (w2l-lines rest-4)
  (splitf-at (cddr rest-3) non-empty-string?))
(define-values (l2t-lines rest-5)
  (splitf-at (cddr rest-4) non-empty-string?))
(define-values (t2h-lines rest-6)
  (splitf-at (cddr rest-5) non-empty-string?))
(define-values (h2l-lines rest-7)
  (splitf-at (cddr rest-6) non-empty-string?))
(define lvls
  (for/list ([lvl (in-list (list s2s-lines s2f-lines f2w-lines w2l-lines l2t-lines t2h-lines h2l-lines))])
    (for/list ([line (in-list lvl)])
      (map string->number (string-split line)))))

(define (next num lvl)
  (cond
    [(null? lvl) num]
    [(and (num . >= . (cadar lvl))
          (num . <= . (+ (cadar lvl) (caddar lvl))))
     (+(- num (cadar lvl)) (caar lvl))]
    [else
     (next num (cdr lvl))]))

(define res
  (apply min
         (map (lambda (seed)
                (for/fold ([acc seed])
                          ([lvl lvls])
                  (next acc lvl)))
              seeds)))

res