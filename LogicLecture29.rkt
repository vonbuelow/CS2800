#lang racket

(require lsl/performance)

; (: isort< (-> (List Number) (List Number)))
(define (isort< lon)
  (local ((define (insert n slon)
  (cond [(empty? slon) (list n)]
        [else
         (if (< n (first slon))
             (cons n slon)
             (cons (first slon) (insert n (rest slon))))])))
    (cond [(empty? lon) '()]
          [else (insert (first lon) (isort< (rest lon)))])))

; (: qsort< (-> (List Number) (List Number)))
(define (qsort< lon)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) lon]
    [(cons? lon)
     (local ((define pivot (first lon))
             (define smallers (filter (lambda (x) (< x pivot)) lon))
             (define equals (filter (lambda (x) (= x pivot)) lon))
             (define biggers (filter (lambda (x) (> x pivot)) lon)))
       (append (qsort< smallers) equals (qsort< biggers)))]))

; (: msort< (-> (List Number) (List Number)))
(define (msort< lon)
  (local ((define (merge lon1 lon2)
            (cond [(and (empty? lon1) (empty? lon2)) '()]
                  [(and (empty? lon1) (cons? lon2)) lon2]
                  [(and (cons? lon1) (empty? lon2)) lon1]
                  [(and (cons? lon1) (cons? lon2)) 
                   (cond [(< (first lon1) (first lon2))
                          (cons (first lon1) 
                                (merge (rest lon1) lon2))]
                         [else ;; (>= (first lon1) (first lon2))
                          (cons (first lon2) 
                                (merge lon1 (rest lon2)))])]))
          (define lenlon (length lon)))
    (if (<= lenlon 1)
        lon
        (local ((define midpoint (quotient lenlon 2))
                (define (take n lon)
                  (cond [(zero? n) empty]
                        [(positive? n) (cons (first lon) (take (sub1 n) (rest lon)))]))
                (define (drop n lon)
                  (cond [(zero? n) lon]
                        [(positive? n) (drop (sub1 n) (rest lon))]))
                (define left (take midpoint lon))
                (define right (drop midpoint lon)))
          (merge (msort< left)
                 (msort< right))))))

(define (mk-list n)
  (build-list n (lambda (_) (random 10000))))

(visualize (build-list 3
                       (λ (n) (mk-list (* n 1000))))
           msort<
           qsort<
           isort<)
