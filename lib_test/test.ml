open OUnit2
open Perf

let rec fact acc = function
  | 1 -> acc
  | n -> fact (n*acc) (n-1)

(* Test that counting cycles returns a positive int64 *)
let count_cycles ctx =
  let c = Perf.(make Attr.(make Cycles)) in
  Perf.enable c;
  let _ = fact 200 in
  Perf.disable c;
  assert_bool "cycles count <= 0" (Perf.read c > 0L)

(* Test Perf.reset *)
let reset ctx =
  let c = Perf.(make Attr.(make Cycles)) in
  Perf.enable c;
  let _ = fact 2 in
  Perf.disable c;
  Perf.reset c;
  assert_equal 0L (Perf.read c)

(* Test Perf.with_process *)
let with_process ctx =
  let r = Perf.with_process
      ~attrs:[Perf.Attr.(make Cycles)] ~cmd:["sleep"; "0"] in
  assert_equal "" r.stdout;
  assert_equal "" r.stderr;
  assert_equal 1 (List.length r.measures);
  let m = List.hd r.measures in
  assert_equal Perf.Attr.Cycles (fst m);
  assert_bool "cycles count <= 0" (snd m > 0L)
  (* assert_bool "cycles count > 2M" (snd m < 2000000L) *)

let suite =
  "suite" >:::
  ["cycles" >:: count_cycles;
   "reset" >:: reset;
   "with_process" >:: with_process;
  ]

let () = run_test_tt_main suite




