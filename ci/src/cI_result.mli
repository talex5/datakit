module Step_log : sig
  type saved = {
    title : string;
    commit : string;
    branch : string;
    failed : bool;
    rebuild : unit Lwt.t Lazy.t;
  }

  type t =
    | Empty
    | Live of CI_live_log.t
    | Saved of saved
    | Pair of t * t
end

type error =
  [ `Failure of string          (* A permanent error (unless an input changes) *)
  | `Pending of string ]        (* A problem that is expected to resolve itself with time *)

type 'a t = ('a, error) result

val pp_error : error Fmt.t
val pp : 'a Fmt.t -> 'a t Fmt.t

