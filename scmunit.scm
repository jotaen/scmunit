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
    (define (check-overview assertions)
        (fold-left (lambda (res a) (string-append res (if (scmunit/assertion-ok? a) "." "x"))) "" assertions))
    (define (check-runtime assertions) (fold-right (lambda (a res) (+ res (scmunit/assertion-runtime a))) 0 assertions))
    (define (testcase-listing testcase)
        (string-append
            "# " (scmunit/testcase-name testcase)
            (if (< 0 (length (scmunit/testcase-assertions testcase)))
                (format #f ": ~A (~Ams)" (check-overview (scmunit/testcase-assertions testcase)) (check-runtime (scmunit/testcase-assertions testcase)))
                "")))
    (define (listing testcases pref) (fold-left (lambda (res tc)
        (string-append res "\n" pref (testcase-listing tc) " " (listing (scmunit/testcase-testcases tc) (string-append pref "  ")))) "" testcases))
    (define (check-verbose assertion:paths) (fold-left (lambda (output:i a:ps)
        (define (concat-paths ps) (fold-right (lambda (p res) (string-append p ": " res)) "" ps))
        (let ((assertion (first a:ps)) (i (second output:i)))
        (list (format #f
            "~A\n~A) ~A\n   - evaluation: ~A -> ~A\n   - assertion: (~A ~A~A)"
            (first output:i)
            i
            (concat-paths (second a:ps))
            (scmunit/assertion-expression assertion)
            (scmunit/assertion-actual assertion)
            (scmunit/assertion-predicate assertion)
            (scmunit/assertion-actual assertion)
            (fold-left (lambda (res a) (format #f "~A ~A" res a)) "" (scmunit/assertion-arguments assertion))) (+ i 1)))) '("" 1) assertion:paths))
    (define (get-checks testcases path) (fold-left (lambda (res g)
        (let ((current-path (append path (list (scmunit/testcase-name g)))))
        (append
            res
            (get-checks (scmunit/testcase-testcases g) current-path)
            (map (lambda (c) (list c current-path)) (scmunit/testcase-assertions g))))) () testcases))
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
