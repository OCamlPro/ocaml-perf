module Attr : sig
  type flag =
    | Disabled (** off by default *)
    | Inherit (** children inherit it *)
    | Exclude_user (** don't count user *)
    | Exclude_kernel (** don't count kernel *)
    | Exclude_hv (** don't count hypervisor *)
    | Exclude_idle (** don't count when idle *)
    | Enable_on_exec (** next exec enables *)

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

  val sexp_of_kind : kind -> Sexplib.Sexp.t
  val kind_of_sexp : Sexplib.Sexp.t -> kind

  type t
  (** Opaque type of a perf event attribute. *)

  val make : ?flags:flag list -> kind -> t
  (** [make ?flags kind] is a perf event attribute of type [kind],
      with flags [flags]. *)

  val kind_of_enum : int -> kind option
  val kind_to_enum : kind -> int
end

type flag =
  | Fd_cloexec (** (since Linux 3.14). This flag enables
                             the close-on-exec flag for the created
                             event file descriptor, so that the file
                             descriptor is automatically closed on
                             execve(2).  Setting the close-on-exec
                             flags at creation time, rather than later
                             with fcntl(2), avoids potential race
                             conditions where the calling thread
                             invokes perf_event_open() and fcntl(2) at
                             the same time as another thread calls
                             fork(2) then execve(2). *)


  | Fd_no_group (** This flag allows creating an event as
                              part of an event group but having no
                              group leader.  It is unclear why this is
                              useful.*)

  | Fd_output (** This flag reroutes the output from an event to the group leader. *)

  | Pid_cgroup (** This flag activates per-container
                             system-wide monitoring.  A container is
                             an abstraction that isolates a set of
                             resources for finer-grained control
                             (CPUs, memory, etc.).  In this mode, the
                             event is measured only if the thread
                             running on the monitored CPU belongs to
                             the designated container (cgroup).  The
                             cgroup is identified by passing a file
                             descriptor opened on its directory in the
                             cgroupfs filesystem.  For instance, if
                             the cgroup to monitor is called test,
                             then a file descriptor opened on
                             /dev/cgroup/test (assuming cgroupfs is
                             mounted on /dev/cgroup) must be passed as
                             the pid parameter.  cgroup monitoring is
                             available only for system-wide events and
                             may therefore require extra
                             permissions. *)

type t
(** Opaque type of an event counter (internally [t] is a file
       descriptor). Each file descriptor corresponds to one event that
       is measured; these can be grouped together to measure multiple
       events simultaneously. *)

val make : ?pid:int -> ?cpu:int -> ?group:t -> ?flags:flag list -> Attr.t -> t
(** [make ~pid ~cpu ?group ?flags attr] is a perf event counter. Refer
    to perf_event_open(2) for the description of the arguments. One
    counter only counts one kind of attribute at a time. If you want
    to simultanously count different metrics (like the perf stat tool
    does), you have to setup several counters. *)

val kind : t -> Attr.kind
(** [kind c] is the kind of events that this counter counts. *)

val read : t -> int64
(** [read c] is the value of the counter [c]. *)

val reset : t -> unit
(** [reset c] sets [c] to zero. *)

val enable : t -> unit
(** Start measuring. *)

val disable : t -> unit
(** Disabling an event. When an event is disabled it does not count or
       generate overflows but does continue to exist and maintain its
       count value. *)

type execution = private {
  return_value: int;
  stdout: string;
  stderr: string;
  data: (Attr.kind * int64) list;
}
(** Type returned by [with_process] *)

val with_process : ?env:string list -> ?timeout:int ->
  string list -> Attr.t list ->
  [`Ok of execution | `Timeout | `Killed | `Exn of exn]
(** [with_process ?env ?timeout cmd attrs] is the result of the
    execution of the program described by [cmd]. This can either be a
    successful execution, or an error. *)

val enable_all : unit -> unit
(** A process can enable or disable all the event groups that are
     attached to it using the prctl(2) PR_TASK_PERF_EVENTS_ENABLE and
     PR_TASK_PERF_EVENTS_DISABLE operations.  This applies to all
     counters on the calling process, whether created by this process
     or by another, and does not affect any counters that this process
     has created on other processes.  It enables or disables only the
     group leaders, not any other members in the groups. *)

val disable_all : unit -> unit
