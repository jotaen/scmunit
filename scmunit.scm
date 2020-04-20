(load-option 'format)

(define *scmunit/testcases* ())

(define-structure scmunit/assertion
    predicate expression actual arguments ok? runtime)

(define-structure scmunit/testcase
    name assertions testcases)

(define-syntax assert
  (syntax-rules () ((_ predicate expression arguments ...)
    (let* (
        (time-start (real-time-clock))
        (actual expression)
        (time-end (real-time-clock))
        (args (list arguments ...))
        (ok? (apply predicate actual args)))
            (make-scmunit/assertion
                'predicate
                'expression
                actual
                args
                ok?
                (* 1000 (internal-time/ticks->seconds (- time-end time-start))))))))

(define (testcase name xs)
    (make-scmunit/testcase
        name
        (filter (lambda (x) (scmunit/assertion? x)) xs)
        (filter (lambda (x) (scmunit/testcase? x)) xs)))

(define (testcase* name xs)
    (set! *scmunit/testcases* (append *scmunit/testcases* (list (testcase name xs)))))

(define (scmunit-run testcases callback)
    (define (check-overview cs)
        (fold-left (lambda (a c) (string-append a (if (scmunit/assertion-ok? c) "." "x"))) "" cs))
    (define (check-runtime cs) (fold-right (lambda (c a) (+ a (scmunit/assertion-runtime c))) 0 cs))
    (define (testcase-listing g)
        (string-append
            "# " (scmunit/testcase-name g)
            (if (< 0 (length (scmunit/testcase-assertions g)))
                (format #f ": ~A (~Ams)" (check-overview (scmunit/testcase-assertions g)) (check-runtime (scmunit/testcase-assertions g)))
                "")))
    (define (listing gs pref) (fold-left (lambda (a g)
        (string-append a "\n" pref (testcase-listing g) " " (listing (scmunit/testcase-testcases g) (string-append pref "  ")))) "" gs))
    (define (check-verbose c:ps) (fold-left (lambda (a:i c:p)
        (define (concat-paths ps) (fold-right (lambda (p a) (string-append p ": " a)) "" ps))
        (let ((c (first c:p)) (ps (second c:p)) (a (first a:i)) (i (second a:i)))
        (list (format #f
            "~A\n~A) ~A  ~A   ->   (~A ~A~A)"
            a
            i
            (concat-paths ps)
            (scmunit/assertion-expression c)
            (scmunit/assertion-predicate c)
            (scmunit/assertion-actual c)
            (fold-left (lambda (x a) (format #f "~A ~A" x a)) "" (scmunit/assertion-arguments c))) (+ i 1)))) '("" 1) c:ps))
    (define (get-checks gs path) (fold-left (lambda (a g)
        (let ((current-path (append path (list (scmunit/testcase-name g)))))
        (append
            a
            (get-checks (scmunit/testcase-testcases g) current-path)
            (map (lambda (c) (list c current-path)) (scmunit/testcase-assertions g))))) () gs))
    (define (summary nr-all nr-failed) (format #f "~A checks ran: ~A passed, ~A failed" nr-all (- nr-all nr-failed) nr-failed))
    (let* (
        (all-checks (get-checks testcases ()))
        (failed-checks (filter (lambda (cp) (not (scmunit/assertion-ok? (first cp)))) all-checks))
        (success? (= 0 (length failed-checks)))
        (output-text (string-append
            (listing testcases "")
            "\n\n"
            (summary (length all-checks) (length failed-checks))
            "\n"
            (first (check-verbose failed-checks))
            "\n"
            (if (not success?) "\n" ""))))
    (begin
        (callback output-text (if (eq? success? #t) 0 1))
        output-text)))

(define (scmunit-run*) (scmunit-run *scmunit/testcases* (lambda (out status) (begin (display out) (exit status)))))
