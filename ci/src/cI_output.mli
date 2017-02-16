type saved = {
  title : string;
  commit : string;
  branch : string;
  failed : bool;
  rebuild : unit Lwt.t Lazy.t;
}

type logs =
  | Empty
  | Live of CI_live_log.t
  | Saved of saved
  | Pair of logs * logs

type 'a t = 'a CI_result.t * logs

val result : 'a t -> 'a CI_result.t
val logs : 'a t -> logs
val status : _ t -> [`Success | `Pending | `Failure]
val descr : string t -> string

val equal : string t -> string t -> bool
(** [equal a b] is [true] iff [a] and [b] are equal for the purposes of saving the output metadata to disk.
    i.e. they have the same result and the same log trees: live logs must be the same object and saved logs
    must be the same commit. *)

val json_of : string t -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> string t
