#lang lsl

#|
Problem 0:

We gave you a UNIQUE code when you walked in the door. Please fill
in the string below with your code, and be sure you include this
definition along with the rest of your code. This confirms that you
were at the exam, as required.
|#

(define CODE "42490")


#|
Problem 1:
Your task in this problem is, give the following formula in propositional logic,
construct a truth table in the form of unit tests. You are welcome to (and
encouraged to) first translate the formula into LSL code. Whether or not
you do so, your tests should run against a definition named `p1`.

(¬(R ∧ Q) ∨ (P ∧ P))
|#


(define (p1 P Q R)
  (or (not (and R Q))
      P))
(check-expect (p1 #t #t #t) #t)
(check-expect (p1 #t #t #f) #t)
(check-expect (p1 #t #f #t) #t)
(check-expect (p1 #t #f #f) #t)

(check-expect (p1 #f #t #t) #f)
(check-expect (p1 #f #t #f) #t)
(check-expect (p1 #f #f #t) #t)
(check-expect (p1 #f #f #f) #t)


#|
Problem 2:
You are an engineer at Young Kids Old Kids Inc. and you are working on your main product:
a tool that calculates the statistical mean (commonly called average) of 
ages given a list of students.

While the standard implementation, student-age-avg (which calculates the mean
by summing all ages and dividing by the count) works, there are concerns
about needing to have all the data before beginning the calculation.

Young Kids Old Kids Inc. is on a rapid growth trajectory, and your CEO, Kiddo Youngster,
is constantly talking about about how to "scale with the ages". You aren't
totally sure what that means, but you're on this startup train, and hoping
to ride it as long as possible.

So, an alternate implementation, student-age-avg-online, has been developed.
This takes two arguments: age and state; the latter (via the struct age-st)
representing the intermediate result. The function returns an updated version
of the age-st struct.

At any point, the current statistical mean of ages can be returned
(it is the mean field), but the count field allows you to incrementalize
the computation. 

Your task, which you hope earns you a promotion, is to write a function
student-age-avg-prop that takes a list of ages (represented as
Natural numbers) and checks that the reference implementation
student-age-avg returns the same result as incrementally calling
student-age-avg-online. The initial version of the struct has been
provided as a constant.

In addition to writing student-age-avg-prop, you should give it a
signature so you can call check-contract on it.

Note that since numeric results can have slight variation, we have given you
a constant DELTA, and suggest you check that the age mean is within DELTA
when computing by the two different mechanisms, rather than checking for equality.

Good luck!
|#


(define DELTA 0.0001)

(define (student-age-avg l)
  (cond [(empty? l) 0]
        [else (/ (apply + l) (length l))]))

(define-struct age-st (mean count))
;; age-st mean and age-st count 

(define AGE-INITIAL (make-age-st 0 0))
;; AGE_INITIAL-mean or AGE-INITIAL-count 

(define (student-age-avg-online age state)
  (let* ([new-count (add1 (age-st-count state))]
         [new-mean (/ (+ (* (age-st-count state)
                            (age-st-mean state))
                         age)
                      new-count)])
  (make-age-st new-mean new-count)))

(: student-age-avg-prop (-> (List Natural) True))
(define (student-age-avg-prop lon)
  (let* ([mean-stu-av (student-age-avg lon)]
         [mean-stu-av-online (student-age-avg-prop-helper lon)])
    (>= DELTA (abs (- mean-stu-av (age-st-mean mean-stu-av-online))))))
(check-contract student-age-avg-prop)

;; return the new mean
(define (student-age-avg-prop-helper lon)
  (let* ([upated student (student-age-avg-online (first lon) AGE-INITIAL-count)])
    (cond [(empty? lon) 0]
          [else (= AGE-INITIAL-mean (student-age-avg-online (first lon)
                                                            (AGE-INITIAL-count)))])))


#|
Problem 3:
Your task in this problem is, give the following truth table, expressed as a set of unit tests,
construct a formula in propositional logic using only `and`, `or`, `not`, and the given
propositional variables. You may not use `if`, `cond`, or any other language construct,
define helper functions, etc. Note: we will assign some partial credit for passing some of
the tests.

Hint: try finding a formula using Q and R for just the first four rows, ignoring
P completely. Now find a formula using Q and R for the last four rows, again
ignoring P. You now have two parts of the formula: figure out how to combine
them together, using P, to get the whole formula.
|#


(define (p3 P Q R)
  (and Q (not R) (not P)))
  

(check-expect (p3 #t #f #f) #f)
(check-expect (p3 #t #f #t) #f)
(check-expect (p3 #t #t #f) #f)
(check-expect (p3 #t #t #t) #f)

(check-expect (p3 #f #f #f) #t)
(check-expect (p3 #f #f #t) #f)
(check-expect (p3 #f #t #f) #f)
(check-expect (p3 #f #t #t) #t)
