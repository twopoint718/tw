(module
  (export "inc" (func $inc))
  (export "dec" (func $dec))
  
  (func $dec (param $i i64) (result i64)
    (if (i64.lt (get_local $i) (i64.const 0))
      (i64.const 0)
      (i64.sub (get_local $i) (i64.const 1))))
      
  (func $inc (param $i i64) (result i64)
    (i64.add (get_local $i) (i64.const 1))))
