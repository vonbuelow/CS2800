#lang lsl

(define-struct file (name contents))
(define-struct directory (name elements))
(define-contract FileSystem (OneOf (File String
                                         String)
                                   (Directory String
                                              (List FileSystem))))
(define FS0 (make-directory "D" (list (make-file "e" "yow!")
                                      (make-file "f" "eek"))))
(define FS1 (make-directory "A" (list (make-file "b" "hello there")
                                      (make-file "c" "goodbye")
                                      FS0
                                      (make-directory "G" (list)))))


(define-struct path [elts])
(define-contract Path~ (Path (List String)))

(: flatten-fs (-> FileSystem (List (File Path~ String))))
(define (flatten-fs fs)
  (cond [(file? fs) (list (make-file (make-path (list (file-name fs))) (file-contents fs)))]
        [(directory? fs)
         (apply append
                (map (lambda (de) (map (lambda (x)
                                         (make-file
                                          (make-path
                                           (cons (directory-name fs)
                                                 (path-elts (file-name x))))
                                          (file-contents x)))
                                       (flatten-fs de)))
                     (directory-elements fs)))]))
(check-expect (flatten-fs FS0) (list (make-file (make-path (list "D" "e")) "yow!")
                                     (make-file (make-path (list "D" "f")) "eek")))
(check-expect (flatten-fs FS1) (list (make-file (make-path (list "A" "b")) "hello there")
                                     (make-file (make-path (list "A" "c")) "goodbye")
                                     (make-file (make-path (list "A" "D" "e")) "yow!")
                                     (make-file (make-path (list "A" "D" "f")) "eek")))

(define-contract CoordState (OneOf Path~
                                   (List (File Path~ String))
                                   False))
(: initial-jobs (-> (List String)
                    (List (SendPacket (File Path~ String)))
                    (List (File Path~ String))
                    (Tuple (List (SendPacket (File Path~ String)))
                           (List (File Path~ String)))))
(define (initial-jobs los ms lof)
  (cond [(or (empty? los)
             (empty? lof)) (list ms lof)]
        [(cons? los) (initial-jobs
                      (rest los)
                      (cons (send-packet
                             (first los)
                             (first lof)) ms)
                      (rest lof))]))
(: coord-start (-> FileSystem
                   (-> (List String) (Action CoordState))))
(define (coord-start fs)
    (λ (los) (action
              (second (initial-jobs los '() (list fs)))
              (first (initial-jobs los '() (list fs))))))

;(: coord-receive (-> (Maybe ~Path))) 
                    
(define (coord-receive path)
  (cond [(path? path) path]
        ;; give another file to look at
        
        [else #f]))

(define-contract WorkerState False)
