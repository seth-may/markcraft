(module
  ;; MarkCraft: WebAssembly matrix operations
  ;; Compile: wat2wasm matrix.wat -o matrix.wasm

  ;; Memory: 1 page (64KB)
  (memory (export "memory") 1)

  ;; Matrix layout: [rows(i32), cols(i32), data(f64...)]
  ;; Offset 0: matrix A, Offset 2048: matrix B, Offset 4096: result

  ;; --- Exports ---
  (func (export "mat_create") (param $offset i32) (param $rows i32) (param $cols i32)
    (i32.store (local.get $offset) (local.get $rows))
    (i32.store (i32.add (local.get $offset) (i32.const 4)) (local.get $cols))
  )

  (func (export "mat_set") (param $offset i32) (param $row i32) (param $col i32) (param $val f64)
    (local $cols i32)
    (local $idx i32)
    (local.set $cols (i32.load (i32.add (local.get $offset) (i32.const 4))))
    (local.set $idx
      (i32.add (i32.const 8)
        (i32.mul (i32.const 8)
          (i32.add (i32.mul (local.get $row) (local.get $cols))
                   (local.get $col)))))
    (f64.store (i32.add (local.get $offset) (local.get $idx)) (local.get $val))
  )

  (func (export "mat_get") (param $offset i32) (param $row i32) (param $col i32) (result f64)
    (local $cols i32)
    (local $idx i32)
    (local.set $cols (i32.load (i32.add (local.get $offset) (i32.const 4))))
    (local.set $idx
      (i32.add (i32.const 8)
        (i32.mul (i32.const 8)
          (i32.add (i32.mul (local.get $row) (local.get $cols))
                   (local.get $col)))))
    (f64.load (i32.add (local.get $offset) (local.get $idx)))
  )

  ;; Matrix multiply: C = A * B
  (func (export "mat_mul")
    (param $a i32) (param $b i32) (param $c i32)
    (local $m i32) (local $n i32) (local $p i32)
    (local $i i32) (local $j i32) (local $k i32)
    (local $sum f64) (local $va f64) (local $vb f64)

    (local.set $m (i32.load (local.get $a)))
    (local.set $n (i32.load (i32.add (local.get $a) (i32.const 4))))
    (local.set $p (i32.load (i32.add (local.get $b) (i32.const 4))))

    ;; Create result matrix
    (i32.store (local.get $c) (local.get $m))
    (i32.store (i32.add (local.get $c) (i32.const 4)) (local.get $p))

    ;; Triple nested loop: O(m*n*p)
    (local.set $i (i32.const 0))
    (block $bi (loop $li
      (br_if $bi (i32.ge_u (local.get $i) (local.get $m)))
      (local.set $j (i32.const 0))
      (block $bj (loop $lj
        (br_if $bj (i32.ge_u (local.get $j) (local.get $p)))
        (local.set $sum (f64.const 0))
        (local.set $k (i32.const 0))
        (block $bk (loop $lk
          (br_if $bk (i32.ge_u (local.get $k) (local.get $n)))
          (local.set $sum (f64.add (local.get $sum)
            (f64.mul
              (call $mat_get_internal (local.get $a) (local.get $i) (local.get $k) (local.get $n))
              (call $mat_get_internal (local.get $b) (local.get $k) (local.get $j) (local.get $p)))))
          (local.set $k (i32.add (local.get $k) (i32.const 1)))
          (br $lk)
        ))
        (call $mat_set_internal (local.get $c) (local.get $i) (local.get $j) (local.get $p) (local.get $sum))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $lj)
      ))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $li)
    ))
  )

  ;; Internal helpers
  (func $mat_get_internal (param $off i32) (param $r i32) (param $c i32) (param $cols i32) (result f64)
    (f64.load (i32.add (local.get $off)
      (i32.add (i32.const 8)
        (i32.mul (i32.const 8)
          (i32.add (i32.mul (local.get $r) (local.get $cols)) (local.get $c)))))))

  (func $mat_set_internal (param $off i32) (param $r i32) (param $c i32) (param $cols i32) (param $v f64)
    (f64.store (i32.add (local.get $off)
      (i32.add (i32.const 8)
        (i32.mul (i32.const 8)
          (i32.add (i32.mul (local.get $r) (local.get $cols)) (local.get $c)))))
      (local.get $v)))
)
