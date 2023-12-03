#lang racket

; Advent of Code 2023 day 1 part 2

(define lines
  (call-with-input-file "01.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        line))))

(define stringnum
  (hash "zero" 0
        "one" 1
        "two" 2
        "three" 3
        "four" 4
        "five" 5
        "six" 6
        "seven" 7
        "eight" 8
        "nine" 9
        "0" 0
        "1" 1
        "2" 2
        "3" 3
        "4" 4
        "5" 5
        "6" 6
        "7" 7
        "8" 8
        "9" 9
        "ight" 8 ; oneight
        "ne" 1 ; twone
        "ine" 9 ; sevenine
        "wo" 2 ; eightwo
        ))

(define (numbers line)
  (regexp-match* #rx"zero|one|two|three|four|five|six|seven|eight|nine|[0-9]|ight|ne|ine|wo" line))

(define (first-last-num line)
  (define mixed-nums (numbers line))
  (define fixed-nums (map (lambda (n) (hash-ref stringnum n)) mixed-nums))
  (define result (+ (* (first fixed-nums) 10) (last fixed-nums)))
  #;(printf "~a ~a ~a\n" result line mixed-nums)
  result)

(for/sum ([l lines])
  (first-last-num l))
