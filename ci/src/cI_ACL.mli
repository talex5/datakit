(** Access control lists. *)

type t =
  [ `Everyone
  | `Username of string
  | `Github_org of string
  | `Any of t list
  ]

val everyone : t
val username : string -> t
val github_org : string -> t
 
val any : t list -> t
