#lang lsl

(define-struct leaf [])
(define-struct node [k v l r])
(define-contract (Tree K V) (OneOf (Leaf) (Node K V (Tree K V) (Tree K V))))

(define-struct sop [empty single union interset member?])
(define-contract (Sop~ Set Elt) (Sop Set
                                     (-> Elt Set)
                                     (-> Set Set Set)
                                     (-> Set Set Set)
                                     (-> Elt Set Boolean)))

(: tree-keys (All (Set V)
                  (-> (Sop~ Set String)
                      (Tree String V)
                      Set)))
(define (tree-keys sop t)
   (if (leaf? t)
       sop-empty
       (tree-help sop t)))

(define (tree-help sop t)
  (if (leaf? t)
      sop-empty
  ((sop-union sop)((sop-union sop) ((sop-single sop) (node-k t))
                   (tree-help sop (node-l t)))
                  (tree-help sop (node-r t)))))
  
       
    ;(sop-single sop) ((λ (x) (if (node? x)
    ;                            (node-k x)
    ;                            "")) t)))

(check-contract tree-keys)

(define SOP-LIST
  (make-sop '()
            (λ (x) '(x))
            (λ (x y)
              (append x y))
            (λ (x y)
              (filter (λ (y)
                        (member? y x))
                      y))
            (λ (x y)
              (member? x y))))

(define T1 (make-node "yellow" "mellow" (make-leaf) (make-leaf)))
(define T1-KEYS (list "yellow"))

(: t1-prop (All (Set)
                (-> (Sop~ Set String)
                    True)))

(define (t1-prop sop)
  (let ([set (tree-keys sop T1)])
    ((sop-member? sop) "yellow" set)))

(check-satisfied SOP-LIST t1-prop)

Now, let’s define a different set implementation; this time, using the idea of characteristic functions: that is, a set of T for some T is a function (-> T Boolean) that return #t for all elements in the set and #f otherwise.

(define SOP-FUN ...)
Now we can again run our same test against this new implementation:

(check-satisfied SOP-FUN t1-prop)

