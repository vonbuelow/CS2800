#lang lsl

;; Problem 1
(: exclusive-range? (-> Integer Integer Integer Boolean))
(define (exclusive-range? lo hi n)
  (and (> hi n) (> n lo)))
(check-contract exclusive-range?)

(: exclusive-range?-prop (-> Integer Integer True))
(define (exclusive-range?-prop lo hi)
  (cond [(>= (add1 lo) hi) #t]
        [(> hi lo)
         (exclusive-range? lo hi (random (+ 1 lo) hi))]))
(check-contract exclusive-range?-prop)

;; Problem 2
(define-contract Odd
  (Immediate (check (λ (x)
                      (and (integer? x) (odd? x))))
             (generate (λ (x) (+ 1 (* 2 (contract-generate Integer x)))))))


(: double-plus1 (-> Odd Odd))
(define (double-plus1 n)
  (+ (* 2 n) 1))
(check-contract double-plus1)

;; Problem 3

(: divisible-by-3-or-5? (-> Integer Boolean))
(define (divisible-by-3-or-5? num)
  (or (= (remainder num 3) 0)
      (= (remainder num 5) 0)))
(check-contract divisible-by-3-or-5?)

(define-contract Divis3or5 (Immediate (check (λ (x)
                                               (or (= (remainder x 3) 0)
                                                   (= (remainder x 5) 0))))))
(: divide-3-or-5 (-> Integer Divis3or5))
(define (divide-3-or-5 num)
  (cond [(divisible-by-3-or-5? num) num]
        [else 0]))
(check-contract divide-3-or-5)