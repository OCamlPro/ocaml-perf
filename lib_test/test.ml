open OUnit2
open Perf

let fact n =
  let rec fact_aux acc = function
    | 1 -> acc
    | n -> fact_aux (n * acc) (n - 1)
  in fact_aux 1 n

(* Test that counting cycles returns a positive int64 *)
let count_cycles _ =
  let c = Perf.(make Attr.(make Kind.Cycles)) in
  Perf.enable c;
  let _ = fact 200 in
  Perf.disable c;
  assert_bool "cycles count <= 0" (Perf.read c > 0L)

(* Test Perf.reset *)
let reset _ =
  let c = Perf.(make Attr.(make Kind.Cycles)) in
  Perf.enable c;
  let _ = fact 2 in
  Perf.disable c;
  Perf.reset c;
  assert_equal 0L (Perf.read c)

(* Test Perf.with_process *)
let with_process _ =
  let r = Perf.with_process ["sleep"; "0"] [Perf.Attr.(make Kind.Cycles)] in
  let exec = match r with
    | `Ok e -> e
    | _ -> failwith "with_process"
  in
  assert_equal "" exec.stdout;
  assert_equal "" exec.stderr;
  assert_equal 1 (KindMap.cardinal exec.data);
  let m = KindMap.min_binding exec.data in
  assert_equal Perf.Attr.Kind.Cycles (fst m);
  assert_bool "cycles count <= 0" (snd m > 0L)

let suite =
  "suite" >:::
  ["cycles" >:: count_cycles;
   "reset" >:: reset;
   "with_process" >:: with_process;
  ]

let () = run_test_tt_main suite




