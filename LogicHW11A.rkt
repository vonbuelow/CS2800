#lang lsl

;; part p0
(define-struct cell (free? value))
(define-contract Cell~ (Cell Boolean Any))

(define-struct memory (pos cells))
(define-contract Memory~ (Memory Natural (List Cell~)))

(define MEMORYSIZE 100)

(define MEMORY
  (make-memory 0 (build-list MEMORYSIZE (lambda (_) (make-cell #t 0)))))
;; part p0


;; Problem 1

;; part p1

;; 1. checking if the free pos is within memory
;; 2. if not see if there is a free cell

(: malloc (-> (Maybe Cell~)))
(define (malloc)
  (cond [(>= (memory-pos MEMORY)(length (memory-cells MEMORY)))
        (findFree)]
        [else (begin
                (set-cell-free?!
                  (list-ref (memory-cells MEMORY)
                  (memory-pos MEMORY)) #f)
                (set-memory-pos! MEMORY
                                 (add1 (memory-pos MEMORY)))
               (list-ref (memory-cells MEMORY)
                  (sub1 (memory-pos MEMORY))))]))



(define (findFree)
  (let ([freeCell (filter (λ (mem) (cell-free? mem))
                            (memory-cells MEMORY))])
    (cond [(>= (length freeCell) 1)
           (begin (set-cell-free?! (first freeCell) #f)
                  (first freeCell))]
          [(zero? (length freeCell))
           #f])))

;; part p1


;; Problem 2

;; part p2
(: free (-> Cell~ False))
(define (free c)
  (begin (set-cell-free?! c #t)
         #f))
;; part p2

;; Problem 3

;; part p3

(: defrag (-> False))
(define (defrag)
  (let ([used (filter (λ (mem) (not (cell-free? mem)))
                            (memory-cells MEMORY))]
        [free (filter (λ (mem) (cell-free? mem))
                            (memory-cells MEMORY))])
    (begin (set-memory-cells! MEMORY (append used free))
           (set-memory-pos! MEMORY (length used))
           #f)))
;; part p3


;; Problem 4

;; part p4a
(define (*= c v)
  (set-cell-value! c v))

(define (deref c) (cell-value c))

(: for-loop (-> Cell~ Natural (-> Any) False))
(define (for-loop idx bound body)
  (if (>= (cell-value idx) bound)
      #f
      (begin (body)
             (set-cell-value! idx (add1 (cell-value idx)))
             (for-loop idx bound body))))
;; part p4a


;; part p4b
(: fib (-> Natural (Maybe Natural)))
(define (fib n)
  (begin
    (define fib1 (malloc))
    (set-cell-free?! fib1 #f)
    (*= fib1 1)
    (define fib2 (malloc))
    (set-cell-free?! fib2 #f)
    (*= fib2 1)
    (define fibonacci (malloc))
    (set-cell-free?! fibonacci #f)
    (*= fibonacci (deref fib1))
    (define index (malloc))
    (set-cell-free?! index #f)
    (*= index 1)
    (for-loop index
              n
              (λ ()
                (begin
                  (*= fibonacci (+ (deref fib1)
                                   (deref fib2)))
                  (*= fib1 (deref fib2))
                  (*= fib2 (deref fibonacci)))))
     (define ret (deref fibonacci))
     (free fib1)
     (free fib2)
     (free fibonacci)
     (free index)
     ret))
         
;; part p4b

(check-expect (fib 0) 1)
(check-expect (fib 1) 1)
(check-expect (fib 2) 2)
(check-expect (fib 3) 3)
(check-expect (fib 4) 5)
(check-expect (fib 5) 8)
(check-expect (fib 6) 13)

;; Problem 5
;;
;; Add contracts above, no code down here.