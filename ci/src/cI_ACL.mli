(** Access control lists. *)

type t =
  [ `Everyone
  | `Username of string
  | `Github_org of string
  | `Can_read of CI_projectID.t
  | `Any of t list
  ]

val everyone : t
val username : string -> t

val github_org : string -> t

val can_read_github : user:string -> project:string -> t
(** [can_read ~user ~project] matches users who can see the [user/project] GitHub repository. *)
 
val any : t list -> t
