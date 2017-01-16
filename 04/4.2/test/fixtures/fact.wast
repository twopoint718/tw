(module
  (export "$fact" (func $fact))
  (export "$main" (func $main))

  (func $fact (param $n i64) (result i64)
    (if i64 (i64.eq (get_local $n) (i64.const 0))
      (i64.const 1)
      (i64.mul
        (get_local $n)
        (call $fact (i64.sub (get_local $n) (i64.const 1))))))

  (func $main (result i64)
    (call $fact (i64.const 10))))

(assert_return (invoke "$main") (i64.const 3628800))
(assert_return (invoke "$fact" (i64.const 5)) (i64.const 120))
