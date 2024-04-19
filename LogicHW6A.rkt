#lang lsl

;; Problem 1

;; part p1a
(define-struct push [num])
(define-struct add [])
(define-struct mul [])
(define-struct sub [])
(define-contract SimpleInstr (OneOf (Push Integer) (Add) (Mul) (Sub)))

(: simple-eval (-> (List Integer) (List SimpleInstr) (List Integer)))
(define (simple-eval stk instrs)
  (local [; stack-binop : [Integer Integer -> Integer] [List-of Integer]
          ;               [List-of SimpleInstr] -> [List-of Integer]
          ; evaluates a binary operator on top two numbers of stack, if present
          (define (stack-binop op stk instrs)
            (if (>= (length stk) 2)
                (simple-eval (cons (op (first stk) (second stk))
                                   (rest (rest stk)))
                             instrs)
                (list)))

          ; eval-instr : Instr [List-of Integer] [List-of SimpleInstr] -> [List-of Integer]
          ; evaluates a single instruction, given a stack and rest of instructions
          (define (eval-instr i stk instrs)
            (cond [(add? i) (stack-binop + stk instrs)]
                  [(mul? i) (stack-binop * stk instrs)]
                  [(sub? i) (stack-binop - stk instrs)]
                  [(push? i) (simple-eval (cons (push-num i) stk) instrs)]))]
    (cond [(empty? instrs) stk]
          [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))
;; part p1a

;; part p1b
(: simple-stack-verify (-> (List SimpleInstr) (List SimpleInstr) Boolean))
(define (simple-stack-verify p1 p2)
  (equal? (simple-eval '() p1)
          (simple-eval '() p2)))
(check-contract simple-stack-verify)

;; part p1b


;; Problem 2

;; part p2
(: simple-const-fold (-> (List SimpleInstr) (List SimpleInstr)))
(define (simple-const-fold p)
  (cond [(empty? p) p]
        [(and (>= (length p) 3)
              (or (add? (third p))
                  (mul? (third p))
                  (sub? (third p)))
              (push? (first p))
              (push? (second p)))
         (cons (make-push (apply (operation (third p)) (list (push-num (first p)) (push-num (second p)))))
               (simple-const-fold (rest (rest (rest p)))))]
        [else (cons (first p) (simple-const-fold (rest p)))]))
(define (operation op)
  (cond [(add? op) +]
        [(mul? op) *]
        [(sub? op) -]))
(check-expect (simple-const-fold (list (make-push 3)
                                       (make-push 5)
                                       (make-push 3)
                                       (make-add)))
              (list (make-push 3) (make-push 8)))
(check-expect (simple-const-fold '()) '())
;; part p2

;; Problem 3

;; part p3
(: simple-const-fold-prop (-> (List SimpleInstr) True))
(define (simple-const-fold-prop p)
  (let ([folded (simple-const-fold p)])
    (simple-stack-verify p folded)))

(check-contract simple-const-fold-prop)

;; part p3

;; Problem 4

;; part p4a
(define-struct var [name])
(define-contract Instr (OneOf (Push Integer) (Add) (Mul) (Sub) (Var String)))

(define-struct bind [name value])
(define-contract Binding (Bind String Integer))


(: eval (-> (List Binding) (List Integer) (List Instr) (List Integer)))
; will return an empty list if it reaches an unbound variable, or a malformed
; program (trying to do an operation without enough values on stack).
(define (eval env stk instrs)
  (local [; stack-binop : [Integer Integer -> Integer] [List-of Integer]
          ;               [List-of Instr] -> [List-of Integer]
          ; evaluates a binary operator on top two numbers of stack, if present
          (define (stack-binop op stk instrs)
            (if (>= (length stk) 2)
                (eval env
                      (cons (op (first stk) (second stk))
                            (rest (rest stk)))
                      instrs)
                (list)))

          ; lookup-var : String [List-of Binding] [List-of Integer]
          ;              [List-of Instr] -> [List-of Integer]
          (define (lookup-var name env stk instrs)
            (cond [(empty? env) (list)]
                  [(cons? env) (if (equal? name (bind-name (first env)))
                                   (eval env
                                         (cons (bind-value (first env))
                                               stk)
                                         instrs)
                                   (lookup-var name (rest env) stk instrs))]))

          ; eval-instr : Instr [List-of Integer] [List-of SimpleInstr] -> [List-of Integer]
          ; evaluates a single instruction, given a stack and rest of instructions
          (define (eval-instr i stk instrs)
            (cond [(add? i) (stack-binop + stk instrs)]
                  [(mul? i) (stack-binop * stk instrs)]
                  [(sub? i) (stack-binop - stk instrs)]
                  [(push? i) (eval env (cons (push-num i) stk) instrs)]
                  [(var? i) (lookup-var (var-name i) env stk instrs)]))]
    (cond [(empty? instrs) stk]
          [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))
;; part p4a

;; Your first task is to first define an updated version of `simple-stack-verify`.
;; This time it will take a substitution (set of variable bindings) that it
;; can pass to `eval`.

;; part p4b
(: stack-verify (-> (List Binding) (List Instr) (List Instr) Boolean))
(define (stack-verify env p1 p2)
  (equal? (eval env '() p1)
          (eval env '() p2)))
(check-contract stack-verify)

;; part p4b

;; part p4c
(: const-fold (-> (List Instr) (List Instr)))
(define (const-fold p)
  (cond [(empty? p) p]
        [(and (>= (length p) 3)
              (or (add? (third p))
                  (mul? (third p))
                  (sub? (third p)))
              (push? (first p))
              (push? (second p)))
         (cons (make-push (apply (operation (third p)) (list (push-num (first p)) (push-num (second p)))))
               (const-fold (rest (rest (rest p)))))]
        [else (cons (first p) (const-fold (rest p)))]))
;; part p4c

;; part p4d
(: const-fold-prop (-> (List Binding) (List Instr) True))
(define (const-fold-prop env p)
  (let ([folded (const-fold p)])
    (stack-verify env p folded)))

(check-contract const-fold-prop)

;; part p4d