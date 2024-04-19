#lang lsl

;; Problem 1:

;; part p1a
(define-contract Bit (OneOf (Constant 0) (Constant 1)))

(define-contract Key (Tuple Bit Bit Bit Bit Bit Bit))

(define-contract Message (Tuple Bit Bit Bit Bit Bit Bit))
;; part p1a

;; part p1b
(: xor (-> Bit Bit Bit))
(define (xor b1 b2)
  (modulo (+ b1 b2) 2))
(check-expect (xor 0 0) 0)
(check-expect (xor 0 1) 1)
(check-expect (xor 1 0) 1)
(check-expect (xor 1 1) 0)

(: xor-list (-> [List Bit] [List Bit] [List Bit]))
(define (xor-list l1 l2)
  (map xor l1 l2))
(check-expect (xor-list (list 1 0 0) (list 1 1 1)) (list 0 1 1))
(check-expect (xor-list (list 0 0 0) (list 0 0 0)) (list 0 0 0))

(: encrypt (-> Message Key Message))
(define encrypt xor-list)

(: decrypt (-> Message Key Message))
(define decrypt xor-list)
;; part p1b

;; part p1c
(: xor-perfect-prop (-> Message Message True))
(define (xor-perfect-prop encr-msg arbitrary-msg)
  (equal? (decrypt encr-msg (encrypt encr-msg arbitrary-msg))
          arbitrary-msg))

(check-contract xor-perfect-prop)
;; part p1c


;; Problem 2


;; part p2a
(define CORRECT-PASSWORD
  (explode "a7he29hdee"))

(define-contract Password (lambda (s) (and (list? s) (andmap string? s))))

(: password=? (-> Password Password Boolean))
(define (password=? l1 l2)
  (password-accumulator l1 l2 #t))

(: password-accumulator (-> Password Password Boolean Boolean))
(define (password-accumulator l1 l2 t)
  (cond [(empty? l1) t]
        [(empty? l2) (password-accumulator l1 l1 t)]
        [else (password-accumulator (rest l1) (rest l2)
                               (and (string=? (first l1)(first l2)) t))]))
         

(: check-password (-> Password Boolean))
(define (check-password p)
  (password=? CORRECT-PASSWORD p))
;; part p2a

;; part p2c
(: timing-spec (-> String String True))
(define (timing-spec s1 s2)
  (let ([p1 (explode s1)]
        [p2 (explode s2)])
    (= (ticks (lambda () (check-password p1)))
       (ticks (lambda () (check-password p2))))))

(check-contract timing-spec)
;; part p2c

boolean before(IWord w); // Determines if the current word comes before another word
 
  IWord checkAndReduceHelp(String s); // Helps check and reduce the word based on a given string
 
  boolean beforeHelp(String s); // Determines if the current word comes before a given string
  
  boolean filterHelp();
