type t =
  [ `Everyone
  | `Username of string
  | `Github_org of string
  | `Can_read of CI_projectID.t
  | `Any of t list
  ]

let everyone = `Everyone
let username x = `Username x
let github_org x = `Github_org x
let can_read_github ~user ~project = `Can_read (CI_projectID.v ~user ~project)
let any ts = `Any ts
