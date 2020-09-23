;; These tests are the passing parts of the WebAssembly test suite:
;; https://github.com/WebAssembly/testsuite
;;
;; At some point we'll be passing the whole test suite and this file won't be
;; needed. Currently used to avoid breakage while refactoring and implementing
;; new features.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                              forward.wast                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module
  (func $even (export "even") (param $n i32) (result i32)
    (if (result i32) (i32.eq (get_local $n) (i32.const 0))
      (then (i32.const 1))
      (else (call $odd (i32.sub (get_local $n) (i32.const 1))))
    )
  )

  (func $odd (export "odd") (param $n i32) (result i32)
    (if (result i32) (i32.eq (get_local $n) (i32.const 0))
      (then (i32.const 0))
      (else (call $even (i32.sub (get_local $n) (i32.const 1))))
    )
  )
)

(assert_return (invoke "even" (i32.const 13)) (i32.const 0))
(assert_return (invoke "even" (i32.const 20)) (i32.const 1))
(assert_return (invoke "odd" (i32.const 13)) (i32.const 1))
(assert_return (invoke "odd" (i32.const 20)) (i32.const 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                              func.wast                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test `func` declarations, i.e. functions

(module
  ;; Auxiliary definition
  (type $sig (func))
  (func $dummy)

  ;; Syntax

  (func)
  (func (export "f"))
  (func $f)
  (func $h (export "g"))

  (func (local))
  (func (local) (local))
  (func (local i32))
  (func (local $x i32))
  (func (local i32 f64 i64))
  (func (local i32) (local f64))
  (func (local i32 f32) (local $x i64) (local) (local i32 f64))

  (func (param))
  (func (param) (param))
  (func (param i32))
  (func (param $x i32))
  (func (param i32 f64 i64))
  (func (param i32) (param f64))
  (func (param i32 f32) (param $x i64) (param) (param i32 f64))

  (func (result i32) (unreachable))

  (type $sig-1 (func))
  (type $sig-2 (func (result i32)))
  (type $sig-3 (func (param $x i32)))
  (type $sig-4 (func (param i32 f64 i32) (result i32)))

  (func (export "type-use-1") (type $sig-1))
  (func (export "type-use-2") (type $sig-2) (i32.const 0))
  (func (export "type-use-3") (type $sig-3))
  (func (export "type-use-4") (type $sig-4) (i32.const 0))
  (func (export "type-use-5") (type $sig-2) (result i32) (i32.const 0))
  (func (export "type-use-6") (type $sig-3) (param i32))
  (func (export "type-use-7")
    (type $sig-4) (param i32) (param f64 i32) (result i32) (i32.const 0)
  )

  (func (type $sig))
  (func (type $forward))  ;; forward reference

  (func $complex
    (param i32 f32) (param $x i64) (param) (param i32)
    (result) (result i32) (result)
    (local f32) (local $y i32) (local i64 i32) (local) (local f64 i32)
    (unreachable) (unreachable)
  )
  (func $complex-sig
    (type $sig)
    (local f32) (local $y i32) (local i64 i32) (local) (local f64 i32)
    (unreachable) (unreachable)
  )

  (type $forward (func))

  ;; Typing of locals

  (func (export "local-first-i32") (result i32) (local i32 i32) (get_local 0))
  (func (export "local-first-i64") (result i64) (local i64 i64) (get_local 0))
  (func (export "local-first-f32") (result f32) (local f32 f32) (get_local 0))
  (func (export "local-first-f64") (result f64) (local f64 f64) (get_local 0))
  (func (export "local-second-i32") (result i32) (local i32 i32) (get_local 1))
  (func (export "local-second-i64") (result i64) (local i64 i64) (get_local 1))
  (func (export "local-second-f32") (result f32) (local f32 f32) (get_local 1))
  (func (export "local-second-f64") (result f64) (local f64 f64) (get_local 1))
  (func (export "local-mixed") (result f64)
    (local f32) (local $x i32) (local i64 i32) (local) (local f64 i32)
    (drop (f32.neg (get_local 0)))
    (drop (i32.eqz (get_local 1)))
    (drop (i64.eqz (get_local 2)))
    (drop (i32.eqz (get_local 3)))
    (drop (f64.neg (get_local 4)))
    (drop (i32.eqz (get_local 5)))
    (get_local 4)
  )

  ;; Typing of parameters

  (func (export "param-first-i32") (param i32 i32) (result i32) (get_local 0))
  (func (export "param-first-i64") (param i64 i64) (result i64) (get_local 0))
  (func (export "param-first-f32") (param f32 f32) (result f32) (get_local 0))
  (func (export "param-first-f64") (param f64 f64) (result f64) (get_local 0))
  (func (export "param-second-i32") (param i32 i32) (result i32) (get_local 1))
  (func (export "param-second-i64") (param i64 i64) (result i64) (get_local 1))
  (func (export "param-second-f32") (param f32 f32) (result f32) (get_local 1))
  (func (export "param-second-f64") (param f64 f64) (result f64) (get_local 1))
  (func (export "param-mixed") (param f32 i32) (param) (param $x i64) (param i32 f64 i32)
    (result f64)
    (drop (f32.neg (get_local 0)))
    (drop (i32.eqz (get_local 1)))
    (drop (i64.eqz (get_local 2)))
    (drop (i32.eqz (get_local 3)))
    (drop (f64.neg (get_local 4)))
    (drop (i32.eqz (get_local 5)))
    (get_local 4)
  )

  ;; Typing of result

  (func (export "empty"))
  (func (export "value-void") (call $dummy))
  (func (export "value-i32") (result i32) (i32.const 77))
  (func (export "value-i64") (result i64) (i64.const 7777))
  (func (export "value-f32") (result f32) (f32.const 77.7))
  (func (export "value-f64") (result f64) (f64.const 77.77))
  (func (export "value-block-void") (block (call $dummy) (call $dummy)))
  (func (export "value-block-i32") (result i32)
    (block (result i32) (call $dummy) (i32.const 77))
  )

  (func (export "return-empty") (return))
  (func (export "return-i32") (result i32) (return (i32.const 78)))
  (func (export "return-i64") (result i64) (return (i64.const 7878)))
  (func (export "return-f32") (result f32) (return (f32.const 78.7)))
  (func (export "return-f64") (result f64) (return (f64.const 78.78)))
  (func (export "return-block-i32") (result i32)
    (return (block (result i32) (call $dummy) (i32.const 77)))
  )

  (func (export "break-empty") (br 0))
  (func (export "break-i32") (result i32) (br 0 (i32.const 79)))
  (func (export "break-i64") (result i64) (br 0 (i64.const 7979)))
  (func (export "break-f32") (result f32) (br 0 (f32.const 79.9)))
  (func (export "break-f64") (result f64) (br 0 (f64.const 79.79)))
  (func (export "break-block-i32") (result i32)
    (br 0 (block (result i32) (call $dummy) (i32.const 77)))
  )

  (func (export "break-br_if-empty") (param i32)
    (br_if 0 (get_local 0))
  )
  (func (export "break-br_if-num") (param i32) (result i32)
    (drop (br_if 0 (i32.const 50) (get_local 0))) (i32.const 51)
  )

  (func (export "break-br_table-empty") (param i32)
    (br_table 0 0 0 (get_local 0))
  )
  (func (export "break-br_table-num") (param i32) (result i32)
    (br_table 0 0 (i32.const 50) (get_local 0)) (i32.const 51)
  )
  (func (export "break-br_table-nested-empty") (param i32)
    (block (br_table 0 1 0 (get_local 0)))
  )
  (func (export "break-br_table-nested-num") (param i32) (result i32)
    (i32.add
      (block (result i32)
        (br_table 0 1 0 (i32.const 50) (get_local 0)) (i32.const 51)
      )
      (i32.const 2)
    )
  )

  ;; Default initialization of locals

  (func (export "init-local-i32") (result i32) (local i32) (get_local 0))
  (func (export "init-local-i64") (result i64) (local i64) (get_local 0))
  (func (export "init-local-f32") (result f32) (local f32) (get_local 0))
  (func (export "init-local-f64") (result f64) (local f64) (get_local 0))
)

(assert_return (invoke "type-use-1"))
(assert_return (invoke "type-use-2") (i32.const 0))
(assert_return (invoke "type-use-3" (i32.const 1)))
(assert_return (invoke "type-use-5") (i32.const 0))
(assert_return (invoke "type-use-6" (i32.const 1)))

(assert_return (invoke "local-first-i32") (i32.const 0))
(assert_return (invoke "local-first-i64") (i64.const 0))
(assert_return (invoke "local-second-i32") (i32.const 0))
(assert_return (invoke "local-second-i64") (i64.const 0))

(assert_return
  (invoke "param-first-i32" (i32.const 2) (i32.const 3)) (i32.const 2)
)
(assert_return
  (invoke "param-first-i64" (i64.const 2) (i64.const 3)) (i64.const 2)
)
(assert_return
  (invoke "param-second-i32" (i32.const 2) (i32.const 3)) (i32.const 3)
)
(assert_return
  (invoke "param-second-i64" (i64.const 2) (i64.const 3)) (i64.const 3)
)

(assert_return (invoke "empty"))
(assert_return (invoke "value-void"))
(assert_return (invoke "value-i32") (i32.const 77))
(assert_return (invoke "value-i64") (i64.const 7777))
(assert_return (invoke "value-block-void"))
(assert_return (invoke "value-block-i32") (i32.const 77))

(assert_return (invoke "return-empty"))
(assert_return (invoke "return-i32") (i32.const 78))
(assert_return (invoke "return-i64") (i64.const 7878))
(assert_return (invoke "return-block-i32") (i32.const 77))

(assert_return (invoke "break-empty"))
(assert_return (invoke "break-i32") (i32.const 79))
(assert_return (invoke "break-i64") (i64.const 7979))
(assert_return (invoke "break-block-i32") (i32.const 77))

(assert_return (invoke "break-br_if-empty" (i32.const 0)))
(assert_return (invoke "break-br_if-empty" (i32.const 2)))
(assert_return (invoke "break-br_if-num" (i32.const 0)) (i32.const 51))
(assert_return (invoke "break-br_if-num" (i32.const 1)) (i32.const 50))

(assert_return (invoke "break-br_table-empty" (i32.const 0)))
(assert_return (invoke "break-br_table-empty" (i32.const 1)))
(assert_return (invoke "break-br_table-empty" (i32.const 5)))
(assert_return (invoke "break-br_table-empty" (i32.const -1)))
(assert_return (invoke "break-br_table-num" (i32.const 0)) (i32.const 50))
(assert_return (invoke "break-br_table-num" (i32.const 1)) (i32.const 50))
(assert_return (invoke "break-br_table-num" (i32.const 10)) (i32.const 50))
(assert_return (invoke "break-br_table-num" (i32.const -100)) (i32.const 50))
(assert_return (invoke "break-br_table-nested-empty" (i32.const 0)))
(assert_return (invoke "break-br_table-nested-empty" (i32.const 1)))
(assert_return (invoke "break-br_table-nested-empty" (i32.const 3)))
(assert_return (invoke "break-br_table-nested-empty" (i32.const -2)))
(assert_return
  (invoke "break-br_table-nested-num" (i32.const 0)) (i32.const 52)
)
(assert_return
  (invoke "break-br_table-nested-num" (i32.const 1)) (i32.const 50)
)
(assert_return
  (invoke "break-br_table-nested-num" (i32.const 2)) (i32.const 52)
)
(assert_return
  (invoke "break-br_table-nested-num" (i32.const -3)) (i32.const 52)
)

(assert_return (invoke "init-local-i32") (i32.const 0))
(assert_return (invoke "init-local-i64") (i64.const 0))


;; Expansion of inline function types

(module
  (func $f (result f64) (f64.const 0))  ;; adds implicit type definition
  (func $g (param i32))                 ;; reuses explicit type definition
  (type $t (func (param i32)))

  (func $i32->void (type 0))                ;; (param i32)
  (func $void->f64 (type 1) (f64.const 0))  ;; (result f64)
  (func $check
    (call $i32->void (i32.const 0))
    (drop (call $void->f64))
  )
)

(module
  (type $sig (func))

  (func $empty-sig-1)  ;; should be assigned type $sig
  (func $complex-sig-1 (param f64 i64 f64 i64 f64 i64 f32 i32))
  (func $empty-sig-2)  ;; should be assigned type $sig
  (func $complex-sig-2 (param f64 i64 f64 i64 f64 i64 f32 i32))
  (func $complex-sig-3 (param f64 i64 f64 i64 f64 i64 f32 i32))
  (func $complex-sig-4 (param i64 i64 f64 i64 f64 i64 f32 i32))
  (func $complex-sig-5 (param i64 i64 f64 i64 f64 i64 f32 i32))

  (type $empty-sig-duplicate (func))
  (type $complex-sig-duplicate (func (param i64 i64 f64 i64 f64 i64 f32 i32)))
  (table anyfunc
    (elem
      $complex-sig-3 $empty-sig-2 $complex-sig-1 $complex-sig-3 $empty-sig-1
      $complex-sig-4 $complex-sig-5
    )
  )

  (func (export "signature-explicit-reused")
    (call_indirect (type $sig) (i32.const 1))
    (call_indirect (type $sig) (i32.const 4))
  )

  (func (export "signature-implicit-reused")
    ;; The implicit index 3 in this test depends on the function and
    ;; type definitions, and may need adapting if they change.
    (call_indirect (type 3)
      (f64.const 0) (i64.const 0) (f64.const 0) (i64.const 0)
      (f64.const 0) (i64.const 0) (f32.const 0) (i32.const 0)
      (i32.const 0)
    )
    (call_indirect (type 3)
      (f64.const 0) (i64.const 0) (f64.const 0) (i64.const 0)
      (f64.const 0) (i64.const 0) (f32.const 0) (i32.const 0)
      (i32.const 2)
    )
    (call_indirect (type 3)
      (f64.const 0) (i64.const 0) (f64.const 0) (i64.const 0)
      (f64.const 0) (i64.const 0) (f32.const 0) (i32.const 0)
      (i32.const 3)
    )
  )

  (func (export "signature-explicit-duplicate")
    (call_indirect (type $empty-sig-duplicate) (i32.const 1))
  )

  (func (export "signature-implicit-duplicate")
    (call_indirect (type $complex-sig-duplicate)
      (i64.const 0) (i64.const 0) (f64.const 0) (i64.const 0)
      (f64.const 0) (i64.const 0) (f32.const 0) (i32.const 0)
      (i32.const 5)
    )
    (call_indirect (type $complex-sig-duplicate)
      (i64.const 0) (i64.const 0) (f64.const 0) (i64.const 0)
      (f64.const 0) (i64.const 0) (f32.const 0) (i32.const 0)
      (i32.const 6)
    )
  )
)

(assert_return (invoke "signature-explicit-reused"))
(assert_return (invoke "signature-implicit-reused"))
(assert_return (invoke "signature-explicit-duplicate"))
(assert_return (invoke "signature-implicit-duplicate"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;                              nop.wast                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test `nop` operator.

(module
  ;; Auxiliary definitions
  (func $dummy)
  (func $3-ary (param i32 i32 i32) (result i32)
    get_local 0 get_local 1 get_local 2 i32.sub i32.add
  )
  (memory 1)

  (func (export "as-func-first") (result i32)
    (nop) (i32.const 1)
  )
  (func (export "as-func-mid") (result i32)
    (call $dummy) (nop) (i32.const 2)
  )
  (func (export "as-func-last") (result i32)
    (call $dummy) (i32.const 3) (nop)
  )
  (func (export "as-func-everywhere") (result i32)
    (nop) (nop) (call $dummy) (nop) (i32.const 4) (nop) (nop)
  )

  (func (export "as-drop-first") (param i32)
    (nop) (get_local 0) (drop)
  )
  (func (export "as-drop-last") (param i32)
    (get_local 0) (nop) (drop)
  )
  (func (export "as-drop-everywhere") (param i32)
    (nop) (nop) (get_local 0) (nop) (nop) (drop)
  )

  (func (export "as-select-first") (param i32) (result i32)
    (nop) (get_local 0) (get_local 0) (get_local 0) (select)
  )
  (func (export "as-select-mid1") (param i32) (result i32)
    (get_local 0) (nop) (get_local 0) (get_local 0) (select)
  )
  (func (export "as-select-mid2") (param i32) (result i32)
    (get_local 0) (get_local 0) (nop) (get_local 0) (select)
  )
  (func (export "as-select-last") (param i32) (result i32)
    (get_local 0) (get_local 0) (get_local 0) (nop) (select)
  )
  (func (export "as-select-everywhere") (param i32) (result i32)
    (nop) (get_local 0) (nop) (nop) (get_local 0)
    (nop) (nop) (get_local 0) (nop) (nop) (select)
  )

  (func (export "as-block-first") (result i32)
    (block (result i32) (nop) (i32.const 2))
  )
  (func (export "as-block-mid") (result i32)
    (block (result i32) (call $dummy) (nop) (i32.const 2))
  )
  (func (export "as-block-last") (result i32)
    (block (result i32) (nop) (call $dummy) (i32.const 3) (nop))
  )
  (func (export "as-block-everywhere") (result i32)
    (block (result i32)
      (nop) (nop) (call $dummy) (nop) (i32.const 4) (nop) (nop)
    )
  )

  (func (export "as-loop-first") (result i32)
    (loop (result i32) (nop) (i32.const 2))
  )
  (func (export "as-loop-mid") (result i32)
    (loop (result i32) (call $dummy) (nop) (i32.const 2))
  )
  (func (export "as-loop-last") (result i32)
    (loop (result i32) (call $dummy) (i32.const 3) (nop))
  )
  (func (export "as-loop-everywhere") (result i32)
    (loop (result i32)
      (nop) (nop) (call $dummy) (nop) (i32.const 4) (nop) (nop)
    )
  )

  (func (export "as-if-condition") (param i32)
    (get_local 0) (nop) (if (then (call $dummy)))
  )
  (func (export "as-if-then") (param i32)
    (if (get_local 0) (then (nop)) (else (call $dummy)))
  )
  (func (export "as-if-else") (param i32)
    (if (get_local 0) (then (call $dummy)) (else (nop)))
  )

  (func (export "as-br-first") (param i32) (result i32)
    (block (result i32) (nop) (get_local 0) (br 0))
  )
  (func (export "as-br-last") (param i32) (result i32)
    (block (result i32) (get_local 0) (nop) (br 0))
  )
  (func (export "as-br-everywhere") (param i32) (result i32)
    (block (result i32) (nop) (nop) (get_local 0) (nop) (nop) (br 0))
  )

  (func (export "as-br_if-first") (param i32) (result i32)
    (block (result i32) (nop) (get_local 0) (get_local 0) (br_if 0))
  )
  (func (export "as-br_if-mid") (param i32) (result i32)
    (block (result i32) (get_local 0) (nop) (get_local 0) (br_if 0))
  )
  (func (export "as-br_if-last") (param i32) (result i32)
    (block (result i32) (get_local 0) (get_local 0) (nop) (br_if 0))
  )
  (func (export "as-br_if-everywhere") (param i32) (result i32)
    (block (result i32)
      (nop) (nop) (get_local 0) (nop) (nop) (get_local 0) (nop) (nop)
      (br_if 0)
    )
  )

  (func (export "as-br_table-first") (param i32) (result i32)
    (block (result i32) (nop) (get_local 0) (get_local 0) (br_table 0 0))
  )
  (func (export "as-br_table-mid") (param i32) (result i32)
    (block (result i32) (get_local 0) (nop) (get_local 0) (br_table 0 0))
  )
  (func (export "as-br_table-last") (param i32) (result i32)
    (block (result i32) (get_local 0) (get_local 0) (nop) (br_table 0 0))
  )
  (func (export "as-br_table-everywhere") (param i32) (result i32)
    (block (result i32)
      (nop) (nop) (get_local 0) (nop) (nop) (get_local 0) (nop) (nop)
      (br_table 0 0)
    )
  )

  (func (export "as-return-first") (param i32) (result i32)
    (nop) (get_local 0) (return)
  )
  (func (export "as-return-last") (param i32) (result i32)
    (get_local 0) (nop) (return)
  )
  (func (export "as-return-everywhere") (param i32) (result i32)
    (nop) (nop) (get_local 0) (nop) (nop) (return)
  )

  (func (export "as-call-first") (param i32 i32 i32) (result i32)
    (nop) (get_local 0) (get_local 1) (get_local 2) (call $3-ary)
  )
  (func (export "as-call-mid1") (param i32 i32 i32) (result i32)
    (get_local 0) (nop) (get_local 1) (get_local 2) (call $3-ary)
  )
  (func (export "as-call-mid2") (param i32 i32 i32) (result i32)
    (get_local 0) (get_local 1) (nop) (get_local 2) (call $3-ary)
  )
  (func (export "as-call-last") (param i32 i32 i32) (result i32)
    (get_local 0) (get_local 1) (get_local 2) (nop) (call $3-ary)
  )
  (func (export "as-call-everywhere") (param i32 i32 i32) (result i32)
    (nop) (nop) (get_local 0) (nop) (nop) (get_local 1)
    (nop) (nop) (get_local 2) (nop) (nop) (call $3-ary)
  )

  (func (export "as-unary-first") (param i32) (result i32)
    (nop) (get_local 0) (i32.ctz)
  )
  (func (export "as-unary-last") (param i32) (result i32)
    (get_local 0) (nop) (i32.ctz)
  )
  (func (export "as-unary-everywhere") (param i32) (result i32)
    (nop) (nop) (get_local 0) (nop) (nop) (i32.ctz)
  )

  (func (export "as-binary-first") (param i32) (result i32)
    (nop) (get_local 0) (get_local 0) (i32.add)
  )
  (func (export "as-binary-mid") (param i32) (result i32)
    (get_local 0) (nop) (get_local 0) (i32.add)
  )
  (func (export "as-binary-last") (param i32) (result i32)
    (get_local 0) (get_local 0) (nop) (i32.add)
  )
  (func (export "as-binary-everywhere") (param i32) (result i32)
    (nop) (get_local 0) (nop) (nop) (get_local 0) (nop) (nop) (i32.add)
  )

  (func (export "as-test-first") (param i32) (result i32)
    (nop) (get_local 0) (i32.eqz)
  )
  (func (export "as-test-last") (param i32) (result i32)
    (get_local 0) (nop) (i32.eqz)
  )
  (func (export "as-test-everywhere") (param i32) (result i32)
    (nop) (nop) (get_local 0) (nop) (nop) i32.eqz
  )

  (func (export "as-compare-first") (param i32) (result i32)
    (nop) (get_local 0) (get_local 0) (i32.ne)
  )
  (func (export "as-compare-mid") (param i32) (result i32)
    (get_local 0) (nop) (get_local 0) (i32.ne)
  )
  (func (export "as-compare-last") (param i32) (result i32)
    (get_local 0) (get_local 0) (nop) (i32.lt_u)
  )
  (func (export "as-compare-everywhere") (param i32) (result i32)
    (nop) (get_local 0) (nop) (nop) (get_local 0) (nop) (nop) (i32.le_s)
  )

  (func (export "as-memory.grow-first") (param i32) (result i32)
    (nop) (get_local 0) (memory.grow)
  )
  (func (export "as-memory.grow-last") (param i32) (result i32)
    (get_local 0) (nop) (memory.grow)
  )
  (func (export "as-memory.grow-everywhere") (param i32) (result i32)
    (nop) (nop) (get_local 0) (nop) (nop) (memory.grow)
  )

  (func $func (param i32 i32) (result i32) (get_local 0))
  (type $check (func (param i32 i32) (result i32)))
  (table anyfunc (elem $func))
  (func (export "as-call_indirect-first") (result i32)
    (block (result i32)
      (nop) (i32.const 1) (i32.const 2) (i32.const 0)
      (call_indirect (type $check))
    )
  )
  (func (export "as-call_indirect-mid1") (result i32)
    (block (result i32)
      (i32.const 1) (nop) (i32.const 2) (i32.const 0)
      (call_indirect (type $check))
    )
  )
  (func (export "as-call_indirect-mid2") (result i32)
    (block (result i32)
      (i32.const 1) (i32.const 2) (nop) (i32.const 0)
      (call_indirect (type $check))
    )
  )
  (func (export "as-call_indirect-last") (result i32)
    (block (result i32)
      (i32.const 1) (i32.const 2) (i32.const 0) (nop)
      (call_indirect (type $check))
    )
  )
  (func (export "as-call_indirect-everywhere") (result i32)
    (block (result i32)
      (nop) (nop) (i32.const 1) (nop) (nop) (i32.const 2) (nop) (nop) (i32.const 0) (nop) (nop)
      (call_indirect (type $check))
    )
  )

  (func (export "as-set_local-first") (param i32) (result i32)
    (nop) (i32.const 2) (set_local 0) (get_local 0)
  )
  (func (export "as-set_local-last") (param i32) (result i32)
    (i32.const 2) (nop) (set_local 0) (get_local 0)
  )
  (func (export "as-set_local-everywhere") (param i32) (result i32)
    (nop) (nop) (i32.const 2) (nop) (nop) (set_local 0) (get_local 0)
  )

  (func (export "as-tee_local-first") (param i32) (result i32)
    (nop) (i32.const 2) (tee_local 0)
  )
  (func (export "as-tee_local-last") (param i32) (result i32)
    (i32.const 2) (nop) (tee_local 0)
  )
  (func (export "as-tee_local-everywhere") (param i32) (result i32)
    (nop) (nop) (i32.const 2) (nop) (nop) (tee_local 0)
  )

  (global $a (mut i32) (i32.const 0))
  (func (export "as-set_global-first") (result i32)
    (nop) (i32.const 2) (set_global $a) (get_global $a)
  )
  (func (export "as-set_global-last") (result i32)
    (i32.const 2) (nop) (set_global $a) (get_global $a)
  )
  (func (export "as-set_global-everywhere") (result i32)
    (nop) (nop) (i32.const 2) (nop) (nop) (set_global 0)
    (get_global $a)
  )

  (func (export "as-load-first") (param i32) (result i32)
    (nop) (get_local 0) (i32.load)
  )
  (func (export "as-load-last") (param i32) (result i32)
    (get_local 0) (nop) (i32.load)
  )
  (func (export "as-load-everywhere") (param i32) (result i32)
    (nop) (nop) (get_local 0) (nop) (nop) (i32.load)
  )

  (func (export "as-store-first") (param i32 i32)
    (nop) (get_local 0) (get_local 1) (i32.store)
  )
  (func (export "as-store-mid") (param i32 i32)
    (get_local 0) (nop) (get_local 1) (i32.store)
  )
  (func (export "as-store-last") (param i32 i32)
    (get_local 0) (get_local 1) (nop) (i32.store)
  )
  (func (export "as-store-everywhere") (param i32 i32)
    (nop) (nop) (get_local 0) (nop) (nop) (get_local 1) (nop) (nop) (i32.store)
  )
)

(assert_return (invoke "as-func-first") (i32.const 1))
(assert_return (invoke "as-func-mid") (i32.const 2))
(assert_return (invoke "as-func-last") (i32.const 3))
(assert_return (invoke "as-func-everywhere") (i32.const 4))

(assert_return (invoke "as-drop-first" (i32.const 0)))
(assert_return (invoke "as-drop-last" (i32.const 0)))
(assert_return (invoke "as-drop-everywhere" (i32.const 0)))

(assert_return (invoke "as-select-first" (i32.const 3)) (i32.const 3))
(assert_return (invoke "as-select-mid1" (i32.const 3)) (i32.const 3))
(assert_return (invoke "as-select-mid2" (i32.const 3)) (i32.const 3))
(assert_return (invoke "as-select-last" (i32.const 3)) (i32.const 3))
(assert_return (invoke "as-select-everywhere" (i32.const 3)) (i32.const 3))

(assert_return (invoke "as-block-first") (i32.const 2))
(assert_return (invoke "as-block-mid") (i32.const 2))
(assert_return (invoke "as-block-last") (i32.const 3))
(assert_return (invoke "as-block-everywhere") (i32.const 4))

(assert_return (invoke "as-loop-first") (i32.const 2))
(assert_return (invoke "as-loop-mid") (i32.const 2))
(assert_return (invoke "as-loop-last") (i32.const 3))
(assert_return (invoke "as-loop-everywhere") (i32.const 4))

(assert_return (invoke "as-if-condition" (i32.const 0)))
(assert_return (invoke "as-if-condition" (i32.const -1)))
(assert_return (invoke "as-if-then" (i32.const 0)))
(assert_return (invoke "as-if-then" (i32.const 4)))
(assert_return (invoke "as-if-else" (i32.const 0)))
(assert_return (invoke "as-if-else" (i32.const 3)))

(assert_return (invoke "as-br-first" (i32.const 5)) (i32.const 5))
(assert_return (invoke "as-br-last" (i32.const 6)) (i32.const 6))
(assert_return (invoke "as-br-everywhere" (i32.const 7)) (i32.const 7))

(assert_return (invoke "as-br_if-first" (i32.const 4)) (i32.const 4))
(assert_return (invoke "as-br_if-mid" (i32.const 5)) (i32.const 5))
(assert_return (invoke "as-br_if-last" (i32.const 6)) (i32.const 6))
(assert_return (invoke "as-br_if-everywhere" (i32.const 7)) (i32.const 7))

(assert_return (invoke "as-br_table-first" (i32.const 4)) (i32.const 4))
(assert_return (invoke "as-br_table-mid" (i32.const 5)) (i32.const 5))
(assert_return (invoke "as-br_table-last" (i32.const 6)) (i32.const 6))
(assert_return (invoke "as-br_table-everywhere" (i32.const 7)) (i32.const 7))

(assert_return (invoke "as-return-first" (i32.const 5)) (i32.const 5))
(assert_return (invoke "as-return-last" (i32.const 6)) (i32.const 6))
(assert_return (invoke "as-return-everywhere" (i32.const 7)) (i32.const 7))

(assert_return (invoke "as-call-first" (i32.const 3) (i32.const 1) (i32.const 2)) (i32.const 2))
(assert_return (invoke "as-call-mid1" (i32.const 3) (i32.const 1) (i32.const 2)) (i32.const 2))
(assert_return (invoke "as-call-mid2" (i32.const 0) (i32.const 3) (i32.const 1)) (i32.const 2))
(assert_return (invoke "as-call-last" (i32.const 10) (i32.const 9) (i32.const -1)) (i32.const 20))
(assert_return (invoke "as-call-everywhere" (i32.const 2) (i32.const 1) (i32.const 5)) (i32.const -2))

(assert_return (invoke "as-unary-first" (i32.const 30)) (i32.const 1))
(assert_return (invoke "as-unary-last" (i32.const 30)) (i32.const 1))
(assert_return (invoke "as-unary-everywhere" (i32.const 12)) (i32.const 2))

(assert_return (invoke "as-binary-first" (i32.const 3)) (i32.const 6))
(assert_return (invoke "as-binary-mid" (i32.const 3)) (i32.const 6))
(assert_return (invoke "as-binary-last" (i32.const 3)) (i32.const 6))
(assert_return (invoke "as-binary-everywhere" (i32.const 3)) (i32.const 6))

(assert_return (invoke "as-test-first" (i32.const 0)) (i32.const 1))
(assert_return (invoke "as-test-last" (i32.const 0)) (i32.const 1))
(assert_return (invoke "as-test-everywhere" (i32.const 0)) (i32.const 1))

(assert_return (invoke "as-compare-first" (i32.const 3)) (i32.const 0))
(assert_return (invoke "as-compare-mid" (i32.const 3)) (i32.const 0))
(assert_return (invoke "as-compare-last" (i32.const 3)) (i32.const 0))
(assert_return (invoke "as-compare-everywhere" (i32.const 3)) (i32.const 1))

(assert_return (invoke "as-memory.grow-first" (i32.const 0)) (i32.const 1))
(assert_return (invoke "as-memory.grow-last" (i32.const 2)) (i32.const 1))
(assert_return (invoke "as-memory.grow-everywhere" (i32.const 12)) (i32.const 3))

(assert_return (invoke "as-call_indirect-first") (i32.const 1))
(assert_return (invoke "as-call_indirect-mid1") (i32.const 1))
(assert_return (invoke "as-call_indirect-mid2") (i32.const 1))
(assert_return (invoke "as-call_indirect-last") (i32.const 1))
(assert_return (invoke "as-call_indirect-everywhere") (i32.const 1))

(assert_return (invoke "as-set_local-first" (i32.const 1)) (i32.const 2))
(assert_return (invoke "as-set_local-last" (i32.const 1)) (i32.const 2))
(assert_return (invoke "as-set_local-everywhere" (i32.const 1)) (i32.const 2))

(assert_return (invoke "as-tee_local-first" (i32.const 1)) (i32.const 2))
(assert_return (invoke "as-tee_local-last" (i32.const 1)) (i32.const 2))
(assert_return (invoke "as-tee_local-everywhere" (i32.const 1)) (i32.const 2))

(assert_return (invoke "as-set_global-first") (i32.const 2))
(assert_return (invoke "as-set_global-last") (i32.const 2))
(assert_return (invoke "as-set_global-everywhere") (i32.const 2))

(assert_return (invoke "as-load-first" (i32.const 100)) (i32.const 0))
(assert_return (invoke "as-load-last" (i32.const 100)) (i32.const 0))
(assert_return (invoke "as-load-everywhere" (i32.const 100)) (i32.const 0))

(assert_return (invoke "as-store-first" (i32.const 0) (i32.const 1)))
(assert_return (invoke "as-store-mid" (i32.const 0) (i32.const 2)))
(assert_return (invoke "as-store-last" (i32.const 0) (i32.const 3)))
(assert_return (invoke "as-store-everywhere" (i32.const 0) (i32.const 4)))
