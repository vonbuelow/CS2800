#lang lsl

;; connections between recursion and induction
;;                     programs and proofs 
;; Khoury hower correspondance

;; inductive hypothesis

;; 

(: subsets-proof (Function (arguments (n Natural))
                           (result (AllOf (List (List Natural))
                                          (lambda (l)
                                            (= (length l)
                                               (expt 2 n)))))))
(define (subsets-proof n)
  (cond [(zero? n) (list (list))]
        [else (let* ([k (sub1 n)]
                     [ksubsets (subsets-proof k)])
                (append ksubsets
                        (map (lambda (s)
                               (cons n s))
                             ksubsets)))]))

(check-contract subsets-proof)

;; algebraic manipulation of a function to prove correctness

(define-contract (TwoFiveProp n)
  (Immediate (check (lambda (t)
                      (= n (+ (* 2 (first t))
                              (* 5 (second t))))))))

(: two-five-proof (Function (arguments (n (AllOf Natural
                                          (lambda (x) (>= x 4)))))
                            (result (AllOf (Tuple Natural Natural)
                                           (TwoFiveProp n)))))
;; look at the data and find a structure of computation

(define (two-five-proof n)
  (cond [(equal? n 4) (list 2 0)]
        [else (let* ([rec (two-five-proof (sub1 n))]
                     [t_k (first rec)]
                     [f_k (second rec)])
                (if (= f_k 0)
                    (list (- t_k 2) 1)
                    (list (+ t_k 3) (- f_k 1))))]))
                     




