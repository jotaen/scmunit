# scmunit: unit testing for Scheme done easy

scmunit is a lightweight test runner and assertion library written in MIT Scheme (R7RS).

## Example

```scheme
(load "scmunit.scm")

(define (increment x) (+ x 1))

(testcase* "increment" (list
    (assert eq? (increment 0) 1)
    (assert eq? (increment 12) 13)
))

(scmunit-run*)
```

## Reference

### `(assert predicate expression [arguments])`

Executes `expression` and passes its return value to `predicate`, along with every `argument` (if there is any). The assertion is considered successful if `predicate` returns `#t`.

### `(testcase name [items])`

Container for grouping assertions and/or other testcases, that are provided as list. Can be nested arbitrarily deep. `name` is a string for recognising the testcase in the test output.

### `(testcase* ...)`

Same as `(testcase ...)`, but it automatically registers the testcases with all its content, so that it gets picked up by the test runner. Supposed to be used for standalone testcases at top-level.

### `(scmunit-run*)`

Runs all testcases that had been registered via `(testcase* ...)`, displays the result and exits the program with status `0` or `1` (depending on whether there were failed tests).

### `(scmunit-run [testcase] callback)`

Runs all test testcases that are passed in and invokes a callback of form `(lambda (text-output status-code) (...))`.

## FAQ

### How can I declare local variables or functions in a testcase?

Provide the list of test items by means of a `let` block:

```scheme
(testcase "magic computation" (let ((MAGIC 42))
    (define (square x) (* x x))
    (list
        (assert eq? (square MAGIC) 1764)
)))
```

### Can test testcases be nested?

Yes, arbitrarily deep:

```scheme
(testcase "foo" (list
    (testcase "bar" (list
        (testcase "baz" (list
            ; ...
        ))
    ))
))
```
