module Attr = struct
  type flag =
    | Disabled [@value 1] (** off by default *)
    | Inherit [@value 2] (** children inherit it *)
    | Exclude_user [@value 4] (** don't count user *)
    | Exclude_kernel [@value 8] (** don't count kernel *)
    | Exclude_hv [@value 16] (** don't count hypervisor *)
    | Exclude_idle [@value 32] (** don't count when idle *)
    | Enable_on_exec [@value 64] (** next exec enables *)
        [@@deriving Enum]

  type kind =
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
        [@@deriving Enum]

  type t = {
    flags: flag list;
    kind: kind
  }
  (** Opaque type of a perf event attribute. *)

  let make ?(flags=[]) kind = { flags; kind; }
  (** [make ?flags kind] is a perf event attribute of type [kind],
      with flags [flags]. *)
end

type flag =
  | Fd_cloexec [@value 1]
  | Fd_no_group [@value 2]
  | Fd_output [@value 4]
  | Pid_cgroup [@value 8]
      [@@deriving Enum]

type t = {
  fd: Unix.file_descr;
  kind: Attr.kind;
}

external perf_event_open : int -> int -> int -> int -> int ->
  int -> Unix.file_descr = "stub_perf_event_open_byte" "stub_perf_event_open"
external perf_event_ioc_enable : Unix.file_descr -> unit = "perf_event_ioc_enable"
external perf_event_ioc_disable : Unix.file_descr -> unit = "perf_event_ioc_disable"
external perf_event_ioc_reset : Unix.file_descr -> unit = "perf_event_ioc_reset"

external enable_all : unit -> unit = "perf_events_enable_all"
external disable_all : unit -> unit = "perf_events_disable_all"

module FlagSet = Set.Make(struct type t = flag let compare = compare end)
module AttrFlagSet = Set.Make(struct type t = Attr.flag let compare = compare end)

let make ?(pid = 0) ?(cpu = -1) ?group ?(flags = []) attr =
  let flags = FlagSet.(of_list flags |> elements) in
  let flags = List.fold_left (fun acc f -> acc + flag_to_enum f) 0 flags in

  let attr_flags = AttrFlagSet.(of_list attr.Attr.flags |> elements) in
  let attr_flags = List.fold_left
      Attr.(fun acc f -> acc + Attr.(flag_to_enum f)) 0 attr_flags in

  let group = match group with
    | None -> -1
    | Some { fd; _ } -> (Obj.magic fd : int) in
  let kind_enum = Attr.(kind_to_enum attr.kind) in
  Attr.{ fd = perf_event_open kind_enum attr_flags pid cpu group flags;
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

type result = {
  measures: (Attr.kind * int64) list;
  stdout: string;
  stderr: string;
}

let string_of_fd fd =
  let buf = Buffer.create 10 in
  let bytes = Bytes.create 4096 in
  let rec drain () =
    match Unix.read fd bytes 0 4096 with
    | 0 -> Buffer.contents buf
    | n -> Buffer.add_subbytes buf bytes 0 n; drain ()
  in drain ()

let with_process ~attrs ~cmd =
  let attrs = List.map Attr.(fun a ->
      { flags = [Disabled; Inherit; Enable_on_exec] @ a.flags;
        kind = a.kind
      }) attrs in
  let counters = List.map make attrs in
  let chd_stdout_read, chd_stdout_write = Unix.pipe () in
  let chd_stderr_read, chd_stderr_write = Unix.pipe () in
  match Unix.fork () with
  | 0 ->
      (* child *)
      Unix.(dup2 chd_stdout_write stdout; close chd_stdout_write);
      Unix.(dup2 chd_stderr_write stderr; close chd_stderr_write);
      Unix.execvp (List.hd cmd) (Array.of_list cmd)
  | n ->
      (* parent *)
      let _ = Unix.waitpid [] n in
      List.iter disable counters;
      Unix.(close chd_stdout_write; close chd_stderr_write);
      {
        measures = List.map (fun c -> c.kind, read c) counters;
        stdout = string_of_fd chd_stdout_read;
        stderr = string_of_fd chd_stderr_read;
      }
