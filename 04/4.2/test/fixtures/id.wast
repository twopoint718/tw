(module
  (func (export "id") (param $x i64) (result i64)
    (return (get_local $x))))
(assert_return (invoke "id" (i64.const 2)) (i64.const 2))
