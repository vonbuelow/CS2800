#lang lsl
(require racket/list)

;; part p1a
(define EMPTY 'empty)
(define WALL 'wall)
(define PLAYER 'player)
(define EXIT 'exit)

(define-contract Cell (OneOf (Constant EMPTY)
                             (Constant WALL)
                             (Constant PLAYER)
                             (Constant EXIT)))
;; part p1a


;; part p1b
(define-struct posn (x y))
(define-struct at (c pos))

(define-contract Posn~ (Posn Natural Natural))

(define-contract Maze (List (At Cell Posn~)))
;; part p1b

;; part p1c
(define-contract SExp (OneOf Symbol
                             Integer
                             Boolean
                             String
                             (List SExp)))
;; part p1c

;; part p3
(define-struct invalid-sexp (sexp))
;; part p3

;; Problem 1
(: random-maze (-> Natural Maze))
(define (random-maze dimension)
  (cond [(zero? dimension) '()]
        [else (list-set (list-set (reverse (maze-generate dimension 0 0 '())) (sub1 (* dimension dimension))
                                  (make-at EXIT (make-posn (sub1 dimension) (sub1 dimension))))
                        0 (make-at PLAYER (make-posn 0 0)))]))

(check-expect (length (random-maze 3)) 9)

(define (maze-generate dimension row col list)
  (cond [(equal? (length list)
                 (* dimension dimension)) list]
        [(equal? dimension col) (maze-generate dimension (add1 row) 0 list)]                                      
        [else
         (maze-generate dimension row (add1 col)
                        (cons (make-at (rand-help)
                        (make-posn row col))
               list))]))

(define (rand-help)
  (let ([rand (random 0 2)])
    (cond [(zero? rand) EMPTY]
          [(= rand 1) WALL])))
 
;; Problem 2
(: single-player-exit? (-> Maze Boolean))
(define (single-player-exit? maze)
  (let ([play-exit (flatten (list-player-exit maze '()))])
    (cond [(or (empty? maze)
               (not (member? PLAYER play-exit))
               (not (member? EXIT play-exit)))
           #f]
          [else (equal? play-exit
                             (remove-duplicates play-exit))])))
(define (list-player-exit m l)
  (cond [(empty? m) l]
        [else (if (or (equal? (at-c (first m)) PLAYER)
                      (equal? (at-c (first m)) EXIT))
                  (list (list-player-exit (rest m) l) (at-c (first m)) l)
                  (list-player-exit (rest m) l))]))
(check-contract single-player-exit?)
(check-expect (single-player-exit? (list (make-at WALL (make-posn 0 0))
                           (make-at PLAYER (make-posn 0 1))
                           (make-at EXIT (make-posn 0 2))
                           (make-at EMPTY (make-posn 0 3))))
              #t)
(check-expect (single-player-exit? (list (make-at PLAYER (make-posn 0 0))
                           (make-at PLAYER (make-posn 0 1))
                           (make-at EXIT (make-posn 0 2))
                           (make-at EMPTY (make-posn 0 3))))
              #f)
(check-expect (single-player-exit? (list (make-at WALL (make-posn 0 0))
                           (make-at PLAYER (make-posn 0 1))
                           (make-at EXIT (make-posn 0 2))
                           (make-at PLAYER (make-posn 0 3))))
              #f)
(check-expect (single-player-exit? (list (make-at WALL (make-posn 0 0))))
              #f)

(: random-maze-2 (-> Natural Maze))
(define (random-maze-2 dimension)
  (let ([maze (random-maze dimension)])
    (if (single-player-exit? maze)
        maze
        (random-maze-2 dimension))))
(check-expect (single-player-exit? (random-maze-2 3)) #t)

;; Problem 3
(: sexp->cell (Function (arguments [_ SExp])
                        (result Cell)
                        (raises invalid-sexp)))
(define (sexp->cell s)
  (cond [(equal? s 'X) WALL]
        [(equal? s '_) EMPTY]
        [(equal? s 'P) PLAYER]
        [(equal? s 'E) EXIT]
        [else (raise (make-invalid-sexp s))]))
(check-contract sexp->cell)
(check-expect (sexp->cell 'X) WALL)
(check-expect (sexp->cell '_) EMPTY)
(check-expect (sexp->cell 'P) PLAYER)
(check-expect (sexp->cell 'E) EXIT)
(check-error (sexp->cell 4))

(: cell->sexp (-> Cell SExp))
(define (cell->sexp c)
  (cond [(equal? EMPTY c) '_]
        [(equal? WALL c) 'X]
        [(equal? PLAYER c) 'P]
        [(equal? EXIT c) 'E]))
(check-contract cell->sexp)
(check-expect (cell->sexp PLAYER) 'P)
(check-expect (cell->sexp EXIT) 'E)
(check-expect (cell->sexp EMPTY) '_)
(check-expect (cell->sexp WALL) 'X)

;; Problem 4
(: cell-roundtrip-prop (-> Cell True))
(define (cell-roundtrip-prop c)
  (equal? c (sexp->cell (cell->sexp c))))
(check-contract cell-roundtrip-prop)

;; Problem 5
#|
(: sexp->maze (Function (arguments [_ SExp])
                        (result Maze)
                        (raises invalid-sexp)))
(define (sexp->maze s)
  (cond [(empty? s) '()]
        [(cons? s) (flatten (sexp->maze-help (first s) (rest s) 0 0))]
        [else (raise (make-invalid-sexp s))]))
(define (sexp->maze-help f s x y)
  (cond [(and (empty? f)
              (empty? s))
         '()]
        [(empty? f) (sexp->maze-help (first s) (rest s) (add1 x) 0)]
        [(cons? f)
         (list (make-at (sexp->cell (first f)) (make-posn x y))
               (sexp->maze-help (rest f) s x (add1 y)))]
        [else (raise (make-invalid-sexp s))]))
(check-expect (sexp->maze (list (list 'P 'X '_)))
              (list (make-at PLAYER (make-posn 0 0))
                    (make-at WALL (make-posn 0 1))
                    (make-at EMPTY (make-posn 0 2))))
(check-expect (sexp->maze (list (list 'X 'X 'P)
                                (list 'E 'X '_)
                                (list '_ '_ '_)))
              (list (make-at WALL (make-posn 0 0))
                    (make-at WALL (make-posn 0 1))
                    (make-at PLAYER (make-posn 0 2))
                    (make-at EXIT (make-posn 1 0))
                    (make-at WALL (make-posn 1 1))
                    (make-at EMPTY (make-posn 1 2))
                    (make-at EMPTY (make-posn 2 0))
                    (make-at EMPTY (make-posn 2 1))
                    (make-at EMPTY (make-posn 2 2))))
(check-error (sexp->maze (list -1 -2)))
(check-error (sexp->maze 4))

(: maze->sexp (-> Maze SExp))
(define (maze->sexp m)
  (let* ([maze (reverse (sort m (λ (x y) (and (<= (posn-x (at-pos x)) (posn-x (at-pos y)))
                                              (<= (posn-y (at-pos x)) (posn-y (at-pos y)))))))])
    (cond [(empty? m) '()]
          [else (list (flatten (maze->sexp-help (first m)
                                                (rest m)
                                                (length maze)
                                                0)))])))

(check-expect (maze->sexp (list (make-at PLAYER (make-posn 0 0))
                                (make-at WALL (make-posn 0 1))
                                (make-at EMPTY (make-posn 0 2))))
              (list (list 'P 'X '_)))
(check-expect (maze->sexp (list (make-at PLAYER (make-posn 1 0))))
              (list (list 'P)))
(check-expect (maze->sexp (list (make-at WALL (make-posn 0 3))
                                (make-at EMPTY (make-posn 0 2))
                                (make-at EXIT (make-posn 0 0))
                                (make-at PLAYER (make-posn 0 1))))
              (list (list 'E 'P '_ 'X)))
(check-expect (maze->sexp (list (make-at PLAYER (make-posn 1 0))))
              (list (list 'X)
                    (list 'P)))

;; Problem 6
(: maze-roundtrip-prop (-> Maze True))
(define (maze-roundtrip-prop m)
  (equal? m (sexp->maze (maze->sexp m))))
;(check-contract maze-roundtrip-prop)

;; Problem 7
(: path-exists? (-> Maze Boolean))  
(define (path-exists? m)
  (cond [(empty? m) #f]
        [(not (single-player-exit? m)) #f]
        
        [else (dfs
               (rest m) (first m)
               '(player-pos m)
               (at-pos (first m)))]))

(check-expect (path-exists? '()) #f)
(check-expect (path-exists? '((list 'X 'X 'X))) #f)

;(: player-pos (-> Maze Posn))
(define (player-pos m)
  ;; cond for empty '()
  (if (equal? PLAYER (at-c (first m)))
      (at-pos (first m))
      (player-pos (rest m))))


(define (dfs m f acc curr)
  (if (or (member? curr acc)
          (equal? WALL (at-c f)))
      #f
      (if (equal? EXIT (at-c f))
          #t
          (dfs m f acc ((neighbors curr))))))


(define (neighbors curr)
  (list (make-posn (sub1 (posn-x curr))
                   (posn-y curr))
        (make-posn (add1 (posn-x curr))
                   (posn-y curr))
        (make-posn (posn-x curr)
                   (sub1 (posn-y curr)))
        (make-posn (posn-x curr)
                   (add1 (posn-y curr)))))

(: random-maze-3 (-> Natural Maze))
(define (random-maze-3 dimension)
  (let ([maze (random-maze-2 dimension)])
    (if (path-exists? maze)
        maze
        (random-maze-3 dimension))))
(check-expect (path-exists? (random-maze-3 4)) #t)

;; Problem 8
(define-contract (Maybe T) (OneOf T (Constant #f)))

(: path-length? (-> Maze (Maybe Natural)))
(define (path-length? maze)
  (if (path-exists? maze)
      (path-helper maze)
      #f))

(define (path-helper maze)
  (define (dfs m f acc curr)
  (if (or (member? curr acc)
          (equal? WALL (at-c f)))
      #f
      (if (equal? EXIT (at-c f))
          #t
          (ormap dfs
                 (neighbors curr))))))

(: random-maze-4 (-> Natural Maze))
(define (random-maze-4 dimension)
  (let ([maze (random-maze-3 dimension)])
    (if (>= (path-length? maze) dimension)
        maze
        (random-maze-4 dimension))))
|#
;; part p8a
;; part p8a
