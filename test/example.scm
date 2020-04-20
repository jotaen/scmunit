(load "scmunit.scm")

(define (abs x) (if (< x 0) (- x) x))

(testcase* "abs" (list

  (testcase "returns non-negative numbers as is" (list
    (assert eq? (abs 0) 0)
    (assert eq? (abs 1) 1)
    (assert eq? (abs 184) 184)
  ))

  (testcase "inverts sign of negative numbers" (list
    (assert eq? (abs -182) 182)
    (assert eq? (abs -1) 1)
  ))

))

(scmunit-run*)

