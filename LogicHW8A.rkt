
#lang lsl
(require racket/math)
(require racket/list)
;; Problem 1

;; part p1
(define-struct tv [T F AND OR NOT])

(define-contract (TruthVal A) (Tv A
                                  A
                                  (-> A A A)
                                  (-> A A A)
                                  (-> A A)))
;; part p1


;; part p1a
(: BOOL-TV (TruthVal Boolean))
(define BOOL-TV (make-tv #t
                         #f
                         (λ (x y) (and x y))
                         (λ (x y) (or x y))
                         (λ (x) (not x))))
;; part p1a

;; Problem 2:


;; part p2
(define-contract Variable Natural)

(define-struct n (var))
;; a negated variable

(define-contract VariableClause (OneOf Variable (N Variable)))
;; a Variable is either a natural number or a negated one

(define-contract (Formula V) (List V))
(define-contract (CNF V) (List (Formula V)))

;; part p2


;; part p2b
(: variable-upper-bound (-> (CNF VariableClause) Variable))
(define (variable-upper-bound cnf)
  (cond [(empty? cnf) 0]
        [(= (length cnf) 1) (if (empty? (first cnf)) 0
                                (first (sort (convert (first cnf) '()) >)))]
        [else (first (sort (big-list (rest cnf) (convert (first cnf) '())) >))]))

(define (big-list cnf acc)
  (cond [(empty? cnf) acc]
        [else (big-list (rest cnf) (append (convert (first cnf) '()) acc))]))

(define (convert f acc)
  (cond [(empty? f) acc]
        [(natural? (first f)) (convert (rest f) (cons (first f) acc))]
        [(n? (first f)) (convert (rest f) (cons (n-var (first f)) acc))]))
       
;; part p2b

;; Problem 3:

;; part p3
(: eval (All (A) (-> (TruthVal A)
                     (CNF A)
                     A)))

(define (eval api cnf)
  (cond [(empty? cnf) (tv-T api)]
        [else ((tv-AND api) (solve-or api (tv-OR api) (first cnf))
                            (solve-or-rest api (tv-OR api) (rest cnf)))]))
(define (solve-or api f cnf)
  (cond [(empty? cnf) (tv-F api)]
        [(> (length cnf) 2)
        (f (f (first cnf) (second cnf))
           (solve-or api f (rest (rest cnf))))]
        [(equal? 1 (length cnf)) (first cnf)]
        [else (apply f cnf)]))

(define (solve-or-rest api f list)
  (cond [(empty? list) (tv-F api)]
        [else (f (solve-or api f (first list))
                 (solve-or-rest api f (rest list)))]))

(test-suite
 "eval"

 (check-expect (eval BOOL-TV (list (list #t #f) (list #f #t))) #t)
 (check-expect (eval BOOL-TV (list (list #t) (list #f))) #f)
 (check-expect (eval BOOL-TV (list (list #t #f #t) (list #f #f #f))) #f)
 (check-expect (eval BOOL-TV (list (list #t #f) (list #f #t) (list #t #t))) #t)
 )

;; part p3


;; Problem 4:

;; part p4
(: subst (All (A) (-> (TruthVal A)
                      (List (Tuple Variable A))
                      (CNF VariableClause)
                      (CNF A))))
(define (subst api vars cnf)
  (cond [(empty? cnf) '(())]
        [else (append (subst-help api vars (first cnf) (rest cnf)))]))

(define (subst-help api vars curr cnf)
  (cond [(empty? cnf) (cons (subst-first api vars curr) '())]
        [else (cons (subst-first api vars curr)
                    (subst-help api vars (first cnf) (rest cnf)))]))

(define (subst-first api vars curr)
  (cond [(empty? curr) '()]
        [else (cons (vars-acc api (first vars) (rest vars) (first curr))
                    (subst-first api vars (rest curr)))]))

(define (vars-acc api f vars curr)
  (cond [(natural? curr) (if (= (first f) curr)
                             (second f)
                             (vars-acc api (first vars) (rest vars) curr))]
        [(n? curr) (if (= (first f) (n-var curr))
                       ((tv-NOT api)(second f))
                       (vars-acc api (first vars) (rest vars) curr))]))

(test-suite
 "subst"

(check-expect (subst BOOL-TV
                     (list (list 4 #t)
                                   (list 9 #f)
                                   (list 8 #t)
                                   (list 5 #f)
                                   (list 2 #t)
                                   (list 7 #t))
                     (list (list 8 (make-n 5))
                           (list 2 9)
                           (list (make-n 7) 4)))
              (list (list #t #t)
                    (list #t #f)
                    (list #f #t)))
(check-expect (subst BOOL-TV
                     (list (list 8 #f)
                           (list 3 #t)
                           (list 9 #f)
                           (list 2 #f))
                     (list (list (make-n 3) 9 8 2)))
              (list (list #f #f #f #f)))

)

;; part p4

;; Problem 5:
;; part p5
(: all-tvs (All (X) (-> X
                        X
                        Variable
                        (List (List (Tuple Variable X))))))
(define (all-tvs T F n)
  (local ((define (all-tvs-local n)
          (cond
            [(zero? n) '(())]
            [else
             (let ([xs (all-tvs-local (sub1 n))])
               (append (map (λ (x) (cons (list (sub1 n) T) x)) xs)
                       (map (λ (x) (cons (list (sub1 n) F) x)) xs)))])))
    (map reverse (all-tvs-local n))))

(test-suite
 "all-tvs"
(check-expect (all-tvs "a" "b" 3)
              '(((0 "a") (1 "a") (2 "a"))
                ((0 "b") (1 "a") (2 "a"))
                ((0 "a") (1 "b") (2 "a"))
                ((0 "b") (1 "b") (2 "a"))
                ((0 "a") (1 "a") (2 "b"))
                ((0 "b") (1 "a") (2 "b"))
                ((0 "a") (1 "b") (2 "b"))
                ((0 "b") (1 "b") (2 "b"))))
             
)

(: sat (All (A) (-> (TruthVal A)
                    (CNF VariableClause)
                    A)))
(define (sat api cnf)
  (eval api (subst api
                   (first (all-tvs (tv-T api) (tv-F api) (add1 (variable-upper-bound cnf))))
                   cnf)))

(test-suite
 "sat"
 (check-expect (sat BOOL-TV
                    (list (list 3 2)
                          (list (make-n 4) (make-n 5))))
               #f)
 (check-expect (sat BOOL-TV
                    (list (list)
                          (list 1 2)))
               #f)
 (check-expect (sat BOOL-TV
                    (list (list 4 0 8)
                          (list (make-n 3) 7 (make-n 9))
                          (list 1 2 (make-n 2))))
               #t)

 )

;; part p5

;; Problem 6:

;; part p6

(: ONEZERO-TV (TruthVal (OneOf (Constant 0) (Constant 1))))
(define ONEZERO-TV (make-tv
                    1
                    0
                    (λ (x y) (if (equal? (and (= x 1) (= y 1)) #t)
                                 1 0))
                    (λ (x y) (if (equal? (or (= x 1) (= y 1)) #t)
                                 1 0))
                    (λ (x) (if (= x 1) 0 1))))

(: STRING-TV (TruthVal String))
(define STRING-TV (make-tv
                   "true"
                   "false"
                   (λ (x y) (if (equal? (and (equal? x "true") (equal? y "true")) #t)
                                "true" "false"))
                   (λ (x y) (if (equal? (or (equal? x "true") (equal? y "true")) #t)
                                "true" "false"))
                   (λ (x) (if (equal? x "true") "false" "true"))))

(define-struct true ())
(define-struct false ())

(: STRUCT-TV (TruthVal (OneOf (True) (False))))
(define STRUCT-TV (make-tv
                   (make-true)
                   (make-false)
                   (λ (x y) (if (equal? (and (true? x) (true? y)) #t)
                                (make-true)
                                (make-false)))
                   (λ (x y) (if (equal? (or (true? x) (true? y)) #t)
                                (make-true)
                                (make-false)))
                   (λ (x) (if (true? x) (make-false) (make-true)))))


(: LAMBDA-TV (TruthVal (All (X) (-> X X X))))
(define LAMBDA-TV (make-tv
                   (λ (x y) x)
                   (λ (x y) y)
                   (λ (a b) (if (equal? (and
                                         (equal? ((λ (x y) x) #t #f) (a #t #f))
                                         (equal? ((λ (x y) x) #t #f) (b #t #f))) #t)
                                (λ (x y) x)
                                (λ (x y) y)))
                   (λ (a b) (if (equal? (or
                                         (equal? ((λ (x y) x) #t #f) (a #t #f))
                                         (equal? ((λ (x y) x) #t #f) (b #t #f))) #t)
                                (λ (x y) x)
                                (λ (x y) y)))
                   (λ (a) (if (equal? ((λ (x y) x) #t #f) (a #t #f))
                              (λ (x y) y)
                              (λ (x y) x)))))
(test-suite
 "sat-other"
 (check-expect (sat ONEZERO-TV
                    (list (list 5 6 7)
                          (list 4 (make-n 3) 2)))
               1)
 (check-expect (sat ONEZERO-TV
                    (list (list 3 6 6)))
               0)

 (check-expect (sat STRING-TV
                    (list (list 1 2 3)
                          (list (make-n 4) 5 6)))
               "true")
 (check-expect (sat STRING-TV
                    (list (list 1 2 3)))
               "false")

 (check-expect (sat STRUCT-TV
                    (list (list (make-n 1) (make-n 2) (make-n 3))
                          (list 4 5 6)))
               (make-false))
 (check-expect (sat STRUCT-TV
                    (list (list 1 2 3)
                          (list (make-n 3) 4 5)))
               (make-true))

 (check-expect ((sat LAMBDA-TV
                     (list (list (make-n 1) (make-n 2) (make-n 3))
                           (list 4 5 6))) #t #f)
               ((λ (x y) y) #t #f))
 (check-expect ((sat LAMBDA-TV
                     (list (list 1 2 3)
                           (list (make-n 3) 4 5))) #t #f)
               ((λ (x y) x) #t #f))
)
;; part p6