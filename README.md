# scmunit: unit testing for MIT Scheme done easy

scmunit is a simple and lightweight test runner plus assertion library written in and for MIT Scheme (R7RS). scmunit comes with the following features:

- 🚀 Small footprint (100 lines of code, 4 kB)
- 🐣 Dead simple API (4 functions – that’s all there is)
- 📝 Concise yet helpful report
- ⏱ Runtime measurements


## Example

```scheme
(load "scmunit.scm")

(define (increment x) (+ x 1))

(testcase* "increments numbers by 1" (list
    (assert eq? (increment 0) 1)
    (assert eq? (increment 12) 13)
))

(scmunit-run*)
```

Also, see another demonstration in [test/example.scm](example.scm).


## Reference

### `(assert predicate expression [arguments])`

Evaluates an expression against a predicate. The assertion is considered to be successful if the predicate returns true (`#t`).

- `predicate` A predicate that takes the result of `expression` as first argument and `arguments` as subsequent arguments
- `expression` The expression that you want to test
- `arguments` See `predicate`

### `(testcase name [items])`

Container for grouping assertions and/or other testcases. Can be nested arbitrarily deep.

- `name` a string for recognising the testcase in the test output of the runner
- `items` a list of assertions and/or testcases

### `(testcase* ...)`

Same as `(testcase ...)`, but it automatically registers the testcases with all its content, so that it gets picked up by the test runner. Supposed to be used at top-level.

### `(scmunit-run*)`

Runs all testcases that had been registered via `(testcase* ...)`, displays the result and exits the program with status `0` or `1` (depending on whether there were failed tests or not).


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
