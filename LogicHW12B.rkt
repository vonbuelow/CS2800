#lang lsl

(define-struct affine-error (v))

(define-struct affine-container (thk))

(define (affine v)
  (let ([a #f])
    (make-affine-container
      (lambda ()
        (if a
            (raise (make-affine-error v))
            (begin (set! a #t)
                   v))))))


(define (affine-get a)
  ((affine-container-thk a)))

(define-contract (Single T)
  (Immediate (check affine-container?)
             (generate (lambda (fuel)
                         (affine (contract-generate T fuel))))))

;; part p0a
(define-struct item (name))
(define-contract Item~ (Item String))
;; part p0a

;; part p0b

(define-contract Inventory (List (Single Item~)))
;; part p0b

;; Problem 1

;; part p1
(: inventory-map (-> (-> Item~ Item~) Inventory Inventory))
(define (inventory-map f i)
  (cond [(empty? i) i]
        [else (map-help f i (list))]))

(define (map-help f i acc)
  (cond [(empty? i) (reverse acc)]
        [else (map-help
               f
               (rest i)
               (cons (affine (f (affine-get (first i))))
                     acc))]))
;; part p1

;; Problem 2

;; part p2
(: inventory-partition (-> (-> Item~ Boolean)
                           Inventory
                           (Tuple Inventory Inventory)))

(define (inventory-partition p? i)
  (cond [(empty? i) (list i i)]
        [else (part-help p? i (list) (list))]))

(define (part-help pred inv t f)
  (if (empty? inv)
      (list (reverse t)
            (reverse f))
      (begin (define val (affine-get (first inv)))
             (if (pred val)
                 (part-help pred
                            (rest inv)
                            (cons (affine val) t)
                            f)
                 (part-help pred
                            (rest inv)
                            t
                            (cons (affine val) f))))))
        
;; part p2


;; Problem 3

;; part p3
(: inventory-get (-> (-> Item~ Boolean)
                     Inventory
                     (Tuple (Maybe (Single Item~)) Inventory)))
(define (inventory-get p? i)
  (let* ([ret (inventory-partition p? i)])
    (if (empty? (first ret))
        (list #f (second ret))
        (list (first (first ret)) (append
                                   (rest (first ret))
                                   (second ret))))))
;; part p3


;; Problem 4
;; part p4
(: craft-hammer (-> Inventory Inventory))
(define (craft-hammer i)
  (if (empty? i)
      i
      (begin (let* ([inv (inventory-get (λ (itm)
                                          (string=? "wood"
                                                    (item-name itm)))
                                        i)]
                    [wood1 (first inv)]
                    [inv2 (inventory-get (λ (itm)
                                           (string=? "wood"
                                                     (item-name itm)))
                                         (second inv))]
                    [wood2 (first inv2)]
                    [inv3 (inventory-get (λ (itm)
                                           (string=? "stone"
                                                     (item-name itm)))
                                         (second inv2))]
                    [stone (first inv3)])
               (if (and (not (boolean? wood1))
                        (not (boolean? wood2))
                        (not (boolean? stone)))
                   (cons (affine (make-item "hammer")) (rest inv3))
                   i)))))

(define Inventory1 (list (affine (make-item "chicken"))
                         (affine (make-item "wood"))
                         (affine (make-item "wood"))
                         (affine (make-item "stone"))))

(check-expect (affine-get (first (craft-hammer Inventory1)))
              (make-item "hammer"))
;; part p4

