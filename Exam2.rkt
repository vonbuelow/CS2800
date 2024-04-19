#lang lsl

#|
Problem 0:

We gave you a UNIQUE code when you walked in the door. Please fill
in the string below with your code, and be sure you include this
definition along with the rest of your code. This confirms that you
were at the exam, as required.
|#

(define CODE "55374")


#|
Problem 1:

Define a function that satisfies the following signature. Write several tests
using the function to confirm that it indeed works without signature violations.
|#

(: c (All (H E Z) (-> (-> H E) (-> E Z) (-> H Z))))

(define (c f g)
  (λ (x) (g (f x))))

(check-expect ((c string->number integer?) "10")
              #t)
(check-expect ((c string->list reverse) "10")
              (list #\0 #\1))
(check-expect ((c length (λ (x) (list x))) (list 1 2 3))
              (list 3))

#|
Problem 2:

Given the following function definition, give it a signature that ensures
that the only thing that it can do is either:
- return its first input, v, unchanged
- return the result of calling its second input, s, on its
  first input v (any number of times)

Your signature should not allow any other behavior.
|#

(: e (All (X Y Z) (-> X (-> X Y) Z (OneOf X
                                          ;; cannot be sure of what
                                          ;; the function returns
                                          ;; but it can be called on X
                                          (-> X (-> X Y) Y)))))
(define (e v s b)
  (if b
      (s v)
      v))

#|
Problem 3:

A tree can be used as a "map" data structure, that stores key->value mappings, as follows:

|#
(define-struct leaf ())
(define-struct node (key value left right))
(define-contract Map (OneOf
                      (Leaf)
                      (Node String Integer Map Map)))

#|
In this map, we allow multiple entries for a given key. i.e., this is a valid
example:
|#

(define M1 (make-node "a" 10
                      (make-leaf)
                      (make-node "a" 20 (make-leaf) (make-leaf))))

#|
We can then define a function called `lookup` that returns _a single_ value for a given
key (or #f if no such key exists); we provide one definition below, but it is not
the only possible one.
|#
(define-contract (Maybe T) (OneOf T (Constant #f)))

(: lookup (-> Map String (Maybe Integer)))
(define (lookup m k)
  (cond [(leaf? m) #f]
        [(node? m)
         (let ([left-val (lookup (node-left m) k)]
               [right-val (lookup (node-right m) k)])
           (cond [(not (boolean? left-val)) left-val]
                 [(not (boolean? right-val)) right-val]
                 [(equal? (node-key m) k) (node-value m)]
                 [else #f]))]))

#|
We would like to write tests for this, but there is an issue with writing
these as unit test if the map contains multiple mappings for a given key. So, instead
of unit tests, we would like you to formulate a property that captures what it means
for `lookup` to work correctly.

You may use, as a helper, the below function `map->list`, which flattens the map into a
list of `key,value` pairs as two-element lists (using `Tuple`).
|#

(: map->list (-> Map (List (Tuple String Integer))))
(define (map->list m)
  (cond [(leaf? m) '()]
        [(node? m) (append (list (list (node-key m) (node-value m)))
                           (map->list (node-left m))
                           (map->list (node-right m)))]))

(define (map-lookup-prop m k)
  (let* ([lookup (lookup m k)]
        [flattened (map->list m)]
        [filter-flat (filter (λ (l) (string=? (first l)
                                              k)) flattened)]
        [val (getvals filter-flat)])
    ;; if the look up value is contained in the list of possible values
    ;; that share the same key
    (if (boolean? lookup)
      #t
      (member? lookup val))))

;; getting all the values of the filter flattened list
(define (getvals l)
  (if (empty? l)
      (list)
      (cons (first l) (getvals (rest l)))))
#|
Problem 4:

The following property (or invariant), written in English, should hold of ormap. Please
translate that into a formal property, in code:

Given a function `fn` and an argument `val`,
if we call `or` on `(fn val)` and the 
 result of calling `ormap` with the same function `fn` on a list `lst`,
this is equivalent to calling `ormap` with the function `fn` on
the list made by consing 
 `val` to `lst`.

|#

(define (ormap-prop fn val lst)
  (equal? (or (fn val)
              (ormap fn lst))
          (ormap fn (cons val lst))))
