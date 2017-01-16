(module
  (export "at_most_five" (func $at_most_five))
  (export "main" (func $main))

  (func $main
    (call $at_most_five (i64.const 5))
    (drop))

  (func $at_most_five (param i64) (result i64)
    (if i64 (i64.gt_u (get_local 0) (i64.const 5))
      (i64.const 5)
      (get_local 0))))

(assert_return (invoke "at_most_five" (i64.const 3)) (i64.const 3))
(assert_return (invoke "at_most_five" (i64.const 9)) (i64.const 5))
(assert_return (invoke "at_most_five" (i64.const 5)) (i64.const 5))
