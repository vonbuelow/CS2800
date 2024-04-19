#lang lsl

;; Problem 1
;; Define the state for the "a" process

;; part p1
(define-contract AState False)
;; part p1

;; Problem 2
;; Define handlers for the "a" process

;; part p2
(: a-start (-> (List String) (Action AState)))
(define (a-start others)
  (action #f (list (send-packet "b" "hello")))) 

(test-suite
 "a-start"
 (check-expect (a-start (list "hellobbbb"))
               (action #f (list (send-packet "b" "hello")))))

(: a-receive (-> AState (ReceivePacket String) (Action AState)))
(define (a-receive st pkt)
  (cond [(and (equal? (receive-packet-from pkt) "c")
              (equal? (receive-packet-msg pkt) "got it"))
         (action #f (list (send-packet "b" "hello")))]
        [else (action #f (list))]))  

(test-suite
 "a-receive"
 (check-expect (a-receive #f (receive-packet "c" "got it"))
               (action #f (list (send-packet "b" "hello"))))
 (check-expect (a-receive #f (receive-packet "c" "yo"))
               (action #f (list))))
;; part p2

;; Problem 3
;; Define state for the "b" process

;; part p3
(define-contract BState False)
;; part p3

;; Problem 4
;; Define handlers for the "b" process

;; part p4
(: b-start (-> (List String) (Action BState)))
(define (b-start others)
  (action #f (list))) 

(test-suite
 "b-start"
 (check-expect (b-start (list "hellobbbb"))
               (action #f (list))))

(: b-receive (-> BState (ReceivePacket String) (Action BState)))
(define (b-receive st pkt)
  (cond [(and (equal? (receive-packet-from pkt) "a")
              (equal? (receive-packet-msg pkt) "hello"))
         (action #f (list (send-packet "c" "hello")))]
        [else (action #f (list))])) 

(test-suite
 "b-receive"
 (check-expect (b-receive #f (receive-packet "a" "hello"))
               (action #f (list (send-packet "c" "hello"))))
 (check-expect (a-receive #f (receive-packet "c" "yo"))
               (action #f (list))))
;; part p4

;; Problem 5
;; Define state for the "c" process

;; part p5
(define-contract CState Natural) ;; how do i change the state of c?
;; part p5

;; Problem 6
;; Define handlers for the "c" process

;; part p6
(: c-start (-> (List String) (Action CState)))
(define (c-start others)
  (action 0 (list))) 

(test-suite
 "c-start"
 (check-expect (c-start (list "hellobbbb"))
               (action 0 (list))))

(: c-receive (-> CState (ReceivePacket String) (Action CState)))
(define (c-receive st pkt)
  (cond [(>= st 4) (action st (list))]
        [(and (equal? (receive-packet-from pkt) "b")
              (equal? (receive-packet-msg pkt) "hello"))
         (action (add1 st)
                 (list (send-packet "a" "got it")))]
        [else (action st (list))]))

(test-suite
 "c-receive"
 (check-expect (c-receive 4 (receive-packet "b" "hello"))
               (action 4 (list)))
 (check-expect (c-receive 2 (receive-packet "b" "hello"))
               (action (add1 2)
                 (list (send-packet "a" "got it")))))
;; part p6

;; Problem 7
;; Define all the processes using the handlers above:

;; part p7
(define a-process
  (process (name "a")
           (on-start a-start)
           (on-receive a-receive)))

(define b-process
  (process (name "b")
           (on-start b-start)
           (on-receive b-receive)))

(define c-process
  (process (name "c")
           (on-start c-start)
           (on-receive c-receive)))
;; part p7

;; Problem 8
;;
;; Define two functions, main and main-debug, that run the program using start
;; and start-debug respectively. You can use `first` as the scheduler for both.

;; part p8
(define (main)
  (start first
         (list a-process
               b-process
               c-process)))

(define (main-debug)
  (start-debug first
               (list a-process
                     b-process
                     c-process)))
;; part p8
