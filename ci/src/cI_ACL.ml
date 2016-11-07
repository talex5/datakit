type t =
  [ `Everyone
  | `Username of string
  | `Github_org of string
  | `Any of t list
  ]

let everyone = `Everyone
let username x = `Username x
let github_org x = `Github_org x
let any ts = `Any ts
