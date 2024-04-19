#lang lsl
;; Problem 1

;; part p1a
(define-struct counter (val))

(: make-counter-1 (-> (-> Natural Natural)))
(define make-counter-1
  (let ([c (make-counter 0)])
    (lambda ()
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))

(: make-counter-2 (-> (-> Natural Natural)))
(define make-counter-2
  (lambda ()
    (let ([c (make-counter 0)])
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))
;; part p1a

;; part p1b
(: counter-distinguish (-> (-> (-> Natural Natural)) Natural))
(define (counter-distinguish fun)
  (let ([count (fun)]
        [count2 (fun)])
    (+ (count 1)
       (count2 1))))

(check-expect (not (equal? (counter-distinguish make-counter-1)
                           (counter-distinguish make-counter-2)))
              #t)
        
;; part p1b

;; Problem 2
;; part p2a
(: fast-incr (-> (Counter Natural) (Counter Natural) Natural))
(define (fast-incr c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))
;; part p2a

;; part p2b
(: fast-incr-prop (-> (Counter Natural) (Counter Natural) True))
(define (fast-incr-prop c1 c2)
  (equal? (+ (counter-val c1) (counter-val c2) 2)
          (fast-incr c1 c2)))
;; part p2b
(define counter1a (make-counter 1))
(define counter1b (make-counter 1))
;; part p2c
(: fast-incr-exercise (-> Natural))
(define (fast-incr-exercise)
  (let ([c (make-counter 1)])
    (fast-incr c c)))
;; part p2c

;; Problem 3

(define-contract (NotEq v1)
	(lambda (v2) (not (eq? v1 v2))))

;; part p3a
(: fast-incr-fixed (Function (arguments [c1 (Counter Natural)]
                                        [c2 (AllOf (Counter Natural)
                                                   (NotEq c1))])
                             (result Natural)))
(define (fast-incr-fixed c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))
;; part p3a

;; Problem 4

;; part p4a
(define-struct mcons (first rest))
(define-contract MList (OneOf empty?
                              (Mcons Integer MList)))
;; part p4a

(define mt empty)
(define cons1 (make-mcons 4 mt))
(define cons2 (make-mcons 5 cons1))
(define cons3 (make-mcons 6 cons2))
(define cons4 (make-mcons 6 cons3))
(set-mcons-rest! cons4 cons4)

; (: maybe-unique2 (-> (AllOf Natural (Record ids))))
(require racket/list)

;; an example of an id which passes the pred. 

(define-contract id
  (lambda (l)
    (equal? (length l)
            (length (remove-duplicates l eq?)))))
(: ids id)
(define ids empty)  
;; part p4b
;;                      
(: mlength (-> (AllOf Any (Record ids)) Natural))
(define (mlength ml)
  (cond [(empty? ml) 0]
        [(mcons? ml) (add1 (mlength (mcons-rest ml)))]))
;; part p4b