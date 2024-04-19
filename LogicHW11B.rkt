#lang racket


;; Problem 1

;; part p1a
(define (min-list l)
  (if (empty? l)
      #f
      (first (sort l <))))

(require lsl/performance)

(visualize (list (list 40 3 5 23 43 23 3 2 1 -94 84)
                 (list 3 -29 4 2 3 21 123 -973)
                 (list 932 34 -83 3 0 3)) min-list)
;; part p1a

;; Problem 2

;; part p2a
(define (maybe-second l)
  (if (greater2 l 0)
      (first (rest l))
      #f))

(define (greater2 l n)
  (if (>= n 2)
      #t
      (if (empty? l)
          #f
          (greater2 (rest l) (+ 1 n)))))
      
;; part p2a

;; Problem 3

;; part p3a
(define (list-reverse l)
  (reverseH l '()))

(define (reverseH l acc)
  (if (empty? l)
      acc
      (reverseH (rest l) (append (list (first l))
                                 acc))))
;; part p3a

;; Problem 4

;; part p4a
(define (fib n)
  (if (<= n 1)
      1
      (+ n n)))
