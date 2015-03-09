module Attr = struct
  type flag =
    | Disabled (** off by default *)
    | Inherit (** children inherit it *)
    | Exclude_user (** don't count user *)
    | Exclude_kernel (** don't count kernel *)
    | Exclude_hv (** don't count hypervisor *)
    | Exclude_idle (** don't count when idle *)
    | Enable_on_exec (** next exec enables *)

  let flag_to_enum = function
    | Disabled -> 1
    | Inherit -> 2
    | Exclude_user -> 4
    | Exclude_kernel -> 8
    | Exclude_hv -> 16
    | Exclude_idle -> 32
    | Enable_on_exec -> 64

  module FSet = Set.Make(struct type t = flag let compare = compare end)

  module Kind = struct
    type t =
      (** Hardware *)
      | Cycles
      | Instructions
      | Cache_references
      | Cache_misses
      | Branch_instructions
      | Branch_misses
      | Bus_cycles
      | Stalled_cycles_frontend
      | Stalled_cycles_backend
      | Ref_cpu_cycles

      (** Software *)
      | Cpu_clock
      | Task_clock
      | Page_faults
      | Context_switches
      | Cpu_migrations
      | Page_faults_min
      | Page_faults_maj
      | Alignment_faults
      | Emulation_faults
      | Dummy

    let to_enum = function
      | Cycles -> 0
      | Instructions -> 1
      | Cache_references -> 2
      | Cache_misses -> 3
      | Branch_instructions -> 4
      | Branch_misses -> 5
      | Bus_cycles -> 6
      | Stalled_cycles_frontend -> 7
      | Stalled_cycles_backend -> 8
      | Ref_cpu_cycles -> 9
      | Cpu_clock -> 10
      | Task_clock -> 11
      | Page_faults -> 12
      | Context_switches -> 13
      | Cpu_migrations -> 14
      | Page_faults_min -> 15
      | Page_faults_maj -> 16
      | Alignment_faults -> 17
      | Emulation_faults -> 18
      | Dummy -> 19


    let sexp_of_t k =
      let open Sexplib.Sexp in
      match k with
      | Cycles -> Atom "Cycles"
      | Instructions -> Atom "Instructions"
      | Cache_references -> Atom "Cache_references"
      | Cache_misses -> Atom "Cache_misses"
      | Branch_instructions -> Atom "Branch_instructions"
      | Branch_misses -> Atom "Branch_misses"
      | Bus_cycles -> Atom "Bus_cycles"
      | Stalled_cycles_frontend -> Atom "Stalled_cycles_frontend"
      | Stalled_cycles_backend -> Atom "Stalled_cycles_backend"
      | Ref_cpu_cycles -> Atom "Ref_cpu_cycles"

      (** Software *)
      | Cpu_clock -> Atom "Cpu_clock"
      | Task_clock -> Atom "Task_clock"
      | Page_faults -> Atom "Page_faults"
      | Context_switches -> Atom "Context_switches"
      | Cpu_migrations -> Atom "Cpu_migrations"
      | Page_faults_min -> Atom "Page_faults_min"
      | Page_faults_maj -> Atom "Page_faults_maj"
      | Alignment_faults -> Atom "Alignment_faults"
      | Emulation_faults -> Atom "Emulation_faults"
      | Dummy -> Atom "Dummy"

    let t_of_sexp s =
      let open Sexplib.Sexp in
      match s with
      | Atom "Cycles" -> Cycles
      | Atom "Instructions" -> Instructions
      | Atom "Cache_references" -> Cache_references
      | Atom "Cache_misses" -> Cache_misses
      | Atom "Branch_instructions" -> Branch_instructions
      | Atom "Branch_misses" -> Branch_misses
      | Atom "Bus_cycles" -> Bus_cycles
      | Atom "Stalled_cycles_frontend" -> Stalled_cycles_frontend
      | Atom "Stalled_cycles_backend" -> Stalled_cycles_backend
      | Atom "Ref_cpu_cycles" -> Ref_cpu_cycles

      (** Software *)
      | Atom "Cpu_clock" -> Cpu_clock
      | Atom "Task_clock" -> Task_clock
      | Atom "Page_faults" -> Page_faults
      | Atom "Context_switches" -> Context_switches
      | Atom "Cpu_migrations" -> Cpu_migrations
      | Atom "Page_faults_min" -> Page_faults_min
      | Atom "Page_faults_maj" -> Page_faults_maj
      | Atom "Alignment_faults" -> Alignment_faults
      | Atom "Emulation_faults" -> Emulation_faults
      | Atom "Dummy" -> Dummy
      | _ -> invalid_arg "kind_of_sexp"

    let to_string t =
      sexp_of_t t |> Sexplib.Sexp.to_string |> String.uncapitalize

    let of_string s =
      String.capitalize s |> Sexplib.Sexp.of_string |> t_of_sexp

    let compare = compare
  end

  type t = {
    flags: FSet.t;
    kind: Kind.t
  }

  let make ?(flags=[]) kind = { flags=FSet.of_list flags; kind; }
  (** [make ?flags kind] is a perf event attribute of type [kind],
      with flags [flags]. *)

  let compare t1 t2 = compare t1.kind t2.kind
end

module KindMap = Map.Make(Attr.Kind)

type flag =
  | Fd_cloexec
  | Fd_no_group
  | Fd_output
  | Pid_cgroup

let flag_to_enum = function
  | Fd_cloexec -> 1
  | Fd_no_group -> 2
  | Fd_output -> 4
  | Pid_cgroup -> 8

type t = {
  fd: Unix.file_descr;
  kind: Attr.Kind.t;
}

external perf_event_open :
  kind:int ->
  attr_flags:int ->
  pid:int ->
  cpu:int ->
  group_fd:int ->
  flags:int ->
  Unix.file_descr = "stub_perf_event_open_byte" "stub_perf_event_open"
external perf_event_ioc_enable : Unix.file_descr -> unit = "perf_event_ioc_enable"
external perf_event_ioc_disable : Unix.file_descr -> unit = "perf_event_ioc_disable"
external perf_event_ioc_reset : Unix.file_descr -> unit = "perf_event_ioc_reset"

external enable_all : unit -> unit = "perf_events_enable_all"
external disable_all : unit -> unit = "perf_events_disable_all"

module FSet = Set.Make(struct type t = flag let compare = compare end)

let make ?(pid = 0) ?(cpu = -1) ?group ?(flags = []) attr =
  let flags = FSet.of_list flags in
  let flags = FSet.fold (fun f acc -> acc + flag_to_enum f) flags 0 in

  let attr_flags =
    let open Attr in
    FSet.fold
      (fun f acc -> acc + Attr.(flag_to_enum f)) attr.flags 0 in

  let group = match group with
    | None -> -1
    | Some { fd; _ } -> (Obj.magic fd : int) in
  let kind_enum = Attr.(Kind.(to_enum attr.kind)) in
  Attr.{ fd = perf_event_open ~kind:kind_enum ~attr_flags ~pid ~cpu
             ~group_fd:group ~flags;
         kind = attr.kind;
       }

let kind c = c.kind

let read c =
  let buf = Bytes.create 8 in
  let nb_read = Unix.read c.fd buf 0 8 in
  assert (nb_read = 8);
  EndianBytes.LittleEndian.get_int64 buf 0

let reset c = perf_event_ioc_reset c.fd
let enable c = perf_event_ioc_enable c.fd
let disable c = perf_event_ioc_disable c.fd
let close c = Unix.close c.fd

type execution = {
  process_status: Unix.process_status;
  stdout: string;
  stderr: string;
  duration: int64;
  data: Int64.t KindMap.t;
}

let string_of_ic ic = really_input_string ic @@ in_channel_length ic

let string_of_file filename =
  let ic = open_in filename in
  try
    let res = string_of_ic ic in close_in ic; res
  with exn ->
    close_in ic; raise exn

let with_process_exn ?env ?timeout ?stdout ?stderr cmd attrs =
  let attrs = List.map Attr.(fun a ->
      { flags = List.fold_left (fun a f -> Attr.FSet.add f a)
            a.flags [Disabled; Inherit; Enable_on_exec];
        kind = a.kind
      }) attrs in
  let counters = List.map make attrs in
  let tmp_stdout_name = match stdout with
    | None -> Filename.temp_file "ocaml-perf" "stdout"
    | Some s -> s
  in
  let tmp_stderr_name = match stderr with
    | None -> Filename.temp_file "ocaml-perf" "stderr"
    | Some s -> s
  in
  let tmp_stdout =
    Unix.(openfile tmp_stdout_name [O_WRONLY; O_CREAT; O_TRUNC] 0o600) in
  let tmp_stderr =
    Unix.(openfile tmp_stderr_name [O_WRONLY; O_CREAT; O_TRUNC] 0o600) in
  let time_start = Oclock.(gettime monotonic) in
  match Unix.fork () with
  | 0 ->
      (* child *)
    Unix.(handle_unix_error
      (fun () ->
         dup2 tmp_stdout stdout; close tmp_stdout;
         dup2 tmp_stderr stderr; close tmp_stderr;
         (match env with
          | None -> execvp (List.hd cmd) (Array.of_list cmd)
          | Some env -> execvpe (List.hd cmd)
                          (Array.of_list cmd) (Array.of_list env))
      ) ())
  | n ->
      (* parent *)
      (* Setup an alarm if timeout is specified. The alarm signal
         handles do nothing, but this will make waitpid fail with
         EINTR, unblocking the program. *)
      let (_:int) = match timeout with None -> 0 | Some t -> Unix.alarm t in
      Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
      let _, process_status = Unix.waitpid [] n in
      List.iter disable counters;
      let time_end = Oclock.(gettime monotonic) in
      Unix.(close tmp_stdout; close tmp_stderr);
      let res =
        {
          process_status;
          stdout = string_of_file tmp_stdout_name;
          stderr = string_of_file tmp_stderr_name;
          duration = Int64.(rem time_end time_start);
          data = List.fold_left
              (fun a c -> KindMap.add c.kind (read c) a)
              KindMap.empty counters;
        }
      in
      List.iter close counters;
      (* Remove stdout/stderr files iff they were left unspecified. *)
      (match stdout with
       | None -> Unix.unlink tmp_stdout_name
       | _ -> ());
      (match stderr with
       | None -> Unix.unlink tmp_stderr_name
       | _ -> ());
      res

let with_process ?env ?timeout ?stdout ?stderr cmd attrs =
  try `Ok (with_process_exn ?env ?timeout ?stdout ?stderr cmd attrs)
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> `Timeout
  | exn -> `Exn exn
