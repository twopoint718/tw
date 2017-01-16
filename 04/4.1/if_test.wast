(module
  (export "if_test" (func $if_test))
  
  (func $if_test (result i32)
    (if i32 (i32.const 1)
      (i32.const 1)
      (i32.const 2))))

(assert_return (invoke "if_test") (i32.const 1))
