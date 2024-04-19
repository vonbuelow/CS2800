#lang lsl

;; includes list contracts 

;; Problem 1
; cyclic-shuffle-prop: String -> #t
(: cyclic-shuffle-prop (-> String True)) ; Contained in the doubled string 
(define (cyclic-shuffle-prop str)
  (string-contains?
   (string-append str str)
   (cyclic-shuffle str))) ;; asking about a equals method
(check-contract cyclic-shuffle-prop 1000)

; cyclic-shuffle: String -> String 
(: cyclic-shuffle (-> String String))
(define (cyclic-shuffle str)
  (let* ([longStr (string-append str str)]
        [rand (random 0 (string-length longStr))])
    (cond [(= rand 0) str]
          [(>= rand (string-length str))
           (substring longStr
                      (- rand (string-length str))
                      rand)]
          [else (substring longStr
                           rand
                           (+ (string-length str)
                              rand))])))

;; Problem 2
; gcd-prop: Natural Natural -> #t
(: gcd-prop (-> Natural Natural True))
(define (gcd-prop nat1 nat2)
  (let ([div (gcd nat1 nat2)])
    (or (and (= 0 nat1)(= 0 nat2))
     (and (= 0 (remainder nat1 div))
         (= 0 (remainder nat2 div))))))
(check-contract gcd-prop)

; gcd: Natural Natural -> Natural
(: gcd (-> Natural Natural Natural))
(define (gcd nat1 nat2)
  (cond [(and (zero? nat1)
             (zero? nat2)) 0]
        [(zero? (modulo nat1 nat2)) nat2]
        [else (gcd (modulo nat1 nat2) nat1)]))
(check-contract gcd)

;; Problem 3
; find-majority-prop: List of Naturals -> #t
(: find-majority-prop (-> (List Natural) True))
(define (find-majority-prop lon)
  (let* ([major-prop (find-majority lon)])
    (or (= major-prop -1)
    (> (length (filter (λ (x) (= x major-prop)) lon))
       (/ (length lon) 2))))) ; the filter should be greater than the lon/2 

(check-contract find-majority-prop)

; find-majority: List of Naturals -> Integer
(: find-majority (-> (List Natural) Integer))
(define (find-majority lon)
  (fm-helper 0 lon))

(define (fm-helper i lon)
  (cond [(= i (length lon)) -1]
        [(> (length
             (filter (λ (x) (= x (list-ref lon i)))
                     lon))
            (/ (length lon) 2)) (list-ref lon i)]
        [else (fm-helper (+ i 1) lon)]))
(check-contract find-majority)