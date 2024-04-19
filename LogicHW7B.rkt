#lang lsl

;; Problem 1


;; part p1a
(define-contract Heap (List Integer))

(: empty-heap Heap)
(define empty-heap empty)
;; part p1a

;; part p1b
(define-contract (Maybe T) (OneOf T (Constant #f)))

(: insert (-> Heap Integer Heap))
(define (insert h n)
  (cond [(or (empty? h)
             (<= n (get-min h))) (cons n h)]
        ;[(equal? n (get-min h)) h]
        [else (cons (get-min h) (insert (rest h) n))]))
(check-contract insert)

(: get-min (-> Heap (Maybe Integer)))
(define (get-min h)
  (if (empty? h)
      #f
      (first h)))
(check-contract get-min)

(: remove-min (-> Heap Heap))
(define (remove-min h)
  (if (empty? h)
      empty-heap
      (rest h)))
(check-contract remove-min)
;; part p1b


;; Problem 2

;; part p2a
(: satisfies-heap-invariant? (-> Heap Boolean))
(define (satisfies-heap-invariant? h)
  (if (empty? h)
      #t
      (equal? h
              (sort h <))))
;; part p2a


;; part p2b
(define-contract CheckedHeap (AllOf Heap satisfies-heap-invariant?))
;; part p2b

;; part p2c
(: insert-checked (-> CheckedHeap Integer CheckedHeap))
(define (insert-checked h n)
  (insert h n))

(: get-min-checked (-> CheckedHeap (Maybe Integer)))
(define (get-min-checked h)
  (get-min h))

(: remove-min-checked (-> CheckedHeap CheckedHeap))
(define (remove-min-checked h)
  (remove-min h))
;; part p2c


;; Problem 3

;; part p3a
(: heap-library (Exists (H)
                        (Tuple H
                               (-> H Integer H)
                               (-> H (Maybe Integer))
                               (-> H H))))
(define heap-library
  (list empty-heap
        insert
        get-min
        remove-min))
;; part p3a