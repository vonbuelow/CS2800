#lang lsl

;; Problem 1
(define-contract PosInt
  (Immediate (check
              (lambda (x)
                (and (integer? x)
                     (or (positive? x)
                         (zero? x)))))
             (generate
              (lambda (x) (contract-generate Integer x)))))

; [list of pos integers] -> true
(: duplicate-element-prop (-> (List Natural) True))
(define (duplicate-element-prop lon)
  (let* ([num (duplicate-element lon)])
    (or (> (length (filter (λ (x) (= num x)) lon)) 1)
        (= num -1))))
(check-contract duplicate-element-prop)

; a list of integers, returns one that appears more than twice
; [list of pos integers] -> integer
; else returns -1
(: duplicate-element (-> (List Natural) Integer))
(define (duplicate-element lon)
  (dupe-helper lon 0))
; sorting the list of positive integers
(define (dupe-helper lon n)
  (cond [(or (>= n (length lon))
             (empty? lon)) -1]
        [(> (length (filter (λ (x) (= x (list-ref lon n))) lon)) 1)
         (list-ref lon n)]
        [else (dupe-helper lon (+ 1 n))]))
; why might unit tests be fragile many correct answers 

;; Problem 2
; formal purpose
(: common-element-prop (-> (List (List Natural)) True))
(define (common-element-prop lol)
  (let ([num (common-element lol)])
    (or (= num -1)
        (andmap (λ (x) (member? num x)) lol))))
(check-contract common-element-prop)


; an element common in each of the lists or 
; [list of [list pos integers]] -> Integer
(: common-element (-> (List (List Natural)) Integer))
(define (common-element lol)
    (cond [(empty? lol) -1]
          [else (ce-helper (first lol) lol 0)]))

(define (ce-helper l lol index)
  (cond [(or (empty? l)
             (>= index (length l))) -1]
        [(andmap (λ (x) (member? (list-ref l index) x)) lol)
         (list-ref l index)]
        [else (ce-helper l lol (+ index 1))]))

;; Problem 3
(: pair-with-sum-prop (-> (List Integer) Integer True))
(define (pair-with-sum-prop lon target)
  (let ([ans (pair-with-sum lon target)])
    (or (empty? ans)
        (= target (foldr + 0 ans)))))
(check-contract pair-with-sum-prop)

(: pair-with-sum (-> (List Integer) Integer (List Integer)))
(define (pair-with-sum lon target)
  (cond [(empty? lon) '()]
        [else (sum-help lon target 0)]))


(define (sum-help lon target index)
  (cond [(>= index (length lon)) '()]
        [(member? target
                  (map (λ (x) (+ (list-ref lon index) x)) lon))
         (index-help lon target index)]
        [else (sum-help lon target (+ 1 index))]))

(define (index-help l target index)
  (list (list-ref l index)
        (first
         (filter
          (λ (x) (= (- target (list-ref l index)) x)) l))))
                  