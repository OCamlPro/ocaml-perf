open Perf

let () =
  if Array.length Sys.argv < 2 then
    (
      Printf.eprintf "Usage: %s <executable>\n" Sys.argv.(0);
      exit 1
    );
  match (with_process
    (List.tl @@ Array.to_list Sys.argv)
    [Attr.(make Kind.Cycles)])
  with
  | `Timeout -> failwith "Timeout"
  | `Exn exn -> raise exn
  | `Ok exec ->
    Printf.printf "%s\n" exec.stdout;
    KindMap.iter (fun k v ->
        Printf.eprintf "%s: %Ld\n" Attr.Kind.(to_string k) v
      )
      exec.data
