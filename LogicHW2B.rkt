#lang lsl

;; Problem 1
(: dist (-> Integer Integer Real))
(define (dist x y)
  (sqrt (+ (expt x 2)
           (expt y 2))))

(check-contract dist)

;; Problem 2
(: cube-vol (-> Real Real)) ; errors on the def. 
(define (cube-vol side-len)
   (expt side-len 3))

(check-contract cube-vol 1000)

;; Problem 3
(: nor (-> Boolean Boolean Boolean))
(define (nor bool1 bool2)
  (and (not bool1)
       (not bool2)))

(check-contract nor)
(check-expect (nor #f #f) #t)

;; Problem 4
(: string-hide (-> String Natural String))
(define (string-hide str nat)
  (cond [(<= nat (string-length str))
         (string-append (substring str 0 (- nat 1))
                        "_" ; the logic of substring & string length 
                        (substring str nat (string-length str)))]
        [else str]))
(check-expect (string-hide "yellow" 10) "yellow")
(check-expect (string-hide "yellow" 3) "ye_low")
(check-contract string-hide)
        