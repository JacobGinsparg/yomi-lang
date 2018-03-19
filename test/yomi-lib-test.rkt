#lang racket

(require rackunit
         "../lib/yomi-lib.rkt")

(test-case
 "frames->seconds"
 (check-equal? 1 (frames->seconds 60 60))
 (check-equal? 1/60 (frames->seconds 1 60))
 (check-equal? 15/60 (frames->seconds 15 60))
 (check-equal? 2/30 (frames->seconds 2 30)))

(test-case
 "link"
 (define move (make-move void 1 2 5 10))
 (check-equal? (make-delay 16/60) (link move 60))
 (check-equal? (make-delay 16/30) (link move 30)))

(test-case
 "cancel"
 (define move (make-move void 1 2 5 10))
 (check-equal? (make-delay 6/60) (cancel move 60))
 (check-equal? (make-delay 6/30) (cancel move 30)))

(test-case
 "perform-combo"
 (define m1-called? #f)
 (define m2-called? #f)
 (define combo (list (make-move (lambda () (set! m1-called? #t)) 1 2 3 4)
                     (make-delay 0)
                     (make-move (lambda () (set! m2-called? #t)) 5 6 7 8)))
 (perform-combo combo)
 (check-true m1-called? "should call the first move's input thunk")
 (check-true m2-called? "should call the second move's input thunk"))

(test-case
 "perform-move"
 (define called? #f)
 (define move (make-move (lambda () (set! called? #t)) 1 2 3 4))
 (perform-move move)
 (check-true called? "should call the move's input thunk"))