(load "scmunit.scm")

; `assertion` gets constructed correctly
(define a (assert eq? (+ 1 3) 4))
(begin
  (if (not (equal? (scmunit/assertion-predicate a) 'eq?)) (error "predicate"))
  (if (not (equal? (scmunit/assertion-expression a) '(+ 1 3))) (error "expression"))
  (if (not (equal? (scmunit/assertion-actual a) 4)) (error "evaluation"))
  (if (not (equal? (scmunit/assertion-arguments a) '(4))) (error "arguments"))
  (if (not (equal? (scmunit/assertion-ok? a) #t)) (error "result"))
  (if (not (>= (scmunit/assertion-runtime a) 0.0)) (error "runtime"))
)

; `testcase` gets constructed correctly
(define tc (testcase "" (list a a (testcase "" '()))))
(begin
  (if (not (= 1 (length (scmunit/testcase-testcases tc)))) (error "testcases"))
  (if (not (= 2 (length (scmunit/testcase-assertions tc)))) (error "assertions"))
)

; assert statement gets evaluated just once
(let ((i 0))
  (define (spy) (set! i (+ i 1)))
  (begin
    (assert (lambda (_) ()) (spy))
    (if (not (= i 1)) (error "nr of invocations (standalone)"))
    (testcase "" (list (assert (lambda (_) ()) (spy))))
    (if (not (= i 2)) (error "nr of invocations (in group)"))
))

(exit 0)
