#lang lsl

;; Problem 1

;; part p1a
(define STARTBAL 1000) ;; each banks starting balance 
(define MAX-TRANSFERS 20)

;; randomly transfer any amount of it to another random bank at beginning
;; recieve a transfer -> sending transfers as long as < MAX-TRANSFERS
;; if maxxed out it can keep accepting but not recieiving
;; Note** think about adding incoming and remoivng transfers

(define-struct bank-state-v1 (balance num-transfers other-banks))
(define-contract BS1 (BankStateV1 Natural Natural (List String)))

(define-struct transfer (bal))
(define-contract Transfer~ (Transfer Natural))
;; part p1a

;; part p1b
;; initial transfers -> another bank in the system
;; note: num transfers is the transfers received so none should be recorded
;; 
(: bank-start-v1 (-> (List String) (Action BS1)))
(define (bank-start-v1 others)
  (let* ([rand (random STARTBAL)])
    (action
     (make-bank-state-v1 (- STARTBAL rand)
                         0
                         others)
     (list (send-packet
            (list-ref others (random (length others)))
            (make-transfer rand))))))
;; part p1b

;; part p1c
;; recieving transfer messages
;; on receipt you add to the balance of current bank
;;            increase the number of transfers
;;            **checking the transfers condition**
;;                       and transfer an amount of the current balance**
(: bank-receive-v1 (-> BS1 (ReceivePacket Transfer~) (Action BS1)))
(define (bank-receive-v1 st pkt)
  (let* ([newBal (+ (transfer-bal (receive-packet-msg pkt))
                  (bank-state-v1-balance st))]
         [withdraw (random newBal)]
         [others (bank-state-v1-other-banks st)])
    (begin (set-bank-state-v1-balance!
               st
               newBal)
           (set-bank-state-v1-num-transfers!
               st
               (add1 (bank-state-v1-num-transfers st)))
           (if (>= (bank-state-v1-num-transfers st) MAX-TRANSFERS)
               (action st (list))
               (begin (set-bank-state-v1-balance!
                       st
                       (- newBal withdraw))
               (action
                st
                (list (send-packet
                      (list-ref others (random (length others)))
                      (make-transfer withdraw)))))))))
              
;; part p1c

;; part p1d
;; takes in a name and makes a bank process with that name 
(define (bank-process-v1 nm)
  (process (name nm)
           (on-start bank-start-v1)
           (on-receive bank-receive-v1)))
;; part p1d

;; part p1e
;; runs the simulation with 5 bank processes 
(define (bank-v1)
  (start first
         (list (bank-process-v1 "b1")
               (bank-process-v1 "b2")
               (bank-process-v1 "b3")
               (bank-process-v1 "b4")
               (bank-process-v1 "b5"))))
;; part p1e


;; Problem 2

;; part p2a
(define UNTIL-SNAPSHOT 10)

(define-struct bank-state (balance num-transfers other-banks snapshot ignored))
(define-contract BS (BankState Natural Natural (List String) (Maybe Natural) (List String)))

(define-struct marker ())

(define-contract Message (OneOf Transfer~ (Marker)))
;; part p2a

;; snapshot recording balances and transfers
;; any new transfers will not be recorded
;; at any point, one party can initiate a snapshot
;; BAL tx ss ignore 

;; part p2b
(: bank-start (-> (List String) (Action BS)))
(define (bank-start others)
  (let* ([rand (random STARTBAL)])
    (action
     (make-bank-state (- STARTBAL rand)
                         0
                         others
                         #f
                         (list))
     (list (send-packet
            (list-ref others (random (length others)))
            (make-transfer rand))))))

(: bank-receive (-> BS (ReceivePacket Message) (Action BS)))
(define (bank-receive st pkt)
  (let* ([up-state (make-bank-state (bank-state-balance st)
                                    (add1 (bank-state-num-transfers st))
                                    (bank-state-other-banks st)
                                    (bank-state-snapshot st)
                                    (bank-state-ignored st))])
    (if (>= (bank-state-num-transfers up-state) MAX-TRANSFERS)
        (cond [(marker? (receive-packet-msg pkt))
               (cond [(natural? (bank-state-snapshot st))
                      (cond [(member? (bank-state-ignored st)
                                      (receive-packet-from pkt))
                             (action up-state (list))]
                            [else
                             (begin (set-bank-state-ignored!
                                     st
                                     (cons (receive-packet-from pkt)
                                           (bank-state-ignored st))))])]
                     [else
                      (cond [(member? (bank-state-ignored st)
                                      (receive-packet-from pkt))
                             (action st (list))]
                            [else
                             (set-bank-state-snapshot!
                              st
                              (bank-state-balance st))])])]
              [(transfer? (receive-packet-msg pkt))
               (cond [(natural? (bank-state-snapshot st))
                      (cond [(member? (bank-state-ignored st)
                                      (receive-packet-from pkt))
                             (action st (list))])])])
        #f))) ;; this would be an additional cond case 



;; generate a list of markers to all of the others
;; map (others (λ)) <- both of the broadcast cases
;; 



;; 8 cases
;; b1 snapshot -> curr balance in ss -> marker to all other banks
;; record current balance

;;              ss     ignored
;; transfer   yes        
;;            no
;; mark:      yes
;;            no
(define (bank-process nm)
  (process (name nm)
           (on-start bank-start)
           (on-receive bank-receive)))

(define (bank)
  (start first
         (list (bank-process "b1")
               (bank-process "b2")
               (bank-process "b3")
               (bank-process "b4")
               (bank-process "b5"))))
;; part p2b

(define (snapshot-correct? b)
  (equal?
   (* STARTBAL (length b))
   (foldr (λ (bnk y)
            (+ (bank-state-snapshot bnk) y))
          0 b )))