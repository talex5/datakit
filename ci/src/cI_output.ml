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

let result = fst
let logs = snd

let status t = CI_result.status (result t)
let descr t = CI_result.descr (result t)

let rec logs_equal a b =
  match a, b with
  | Empty, Empty -> true
  | Live a, Live b -> a == b
  | Saved a, Saved b -> a.commit = b.commit
  | Pair (a1, a2), Pair (b1, b2) -> logs_equal a1 b1 && logs_equal a2 b2
  | _ -> false

let equal a b =
  result a = result b && logs_equal (logs a) (logs b)

let rec json_of_logs : logs -> Yojson.Basic.json = function
  | Empty -> `Null
  | Live x ->
    `Assoc [
      "branch", `String (CI_live_log.branch x);
    ]
  | Saved x ->
    `Assoc [
      "title", `String x.title;
      "branch", `String x.branch;
      "commit", `String x.commit;
      "failed", `Bool x.failed;
    ]
  | Pair (a, b) ->
    match json_of_logs a, json_of_logs b with
    | `Null, x -> x
    | x, `Null -> x
    | x, y ->
      `List [x; y]

let rec logs_of_json = function
  | `Null -> Empty
  | `Assoc [
      "branch", `String _x;
    ] -> Empty  (* Can't restore live logs currently *)
  | `Assoc [
      "title", `String title;
      "branch", `String branch;
      "commit", `String commit;
      "failed", `Bool failed;
    ] -> Saved { title; commit; branch; failed; rebuild = lazy Lwt.return_unit }
  | `List [a; b] -> Pair (logs_of_json a, logs_of_json b)
  | json -> CI_utils.failf "Invalid logs JSON: %a" (Yojson.Basic.pretty_print ?std:None) json

let json_of (result, logs) =
  `Assoc [
    "result", CI_result.json_of result;
    "logs", json_of_logs logs;
  ]

let of_json = function
  | `Assoc [
      "result", result;
      "logs", logs;
    ] -> (CI_result.of_json result, logs_of_json logs)
  | json -> CI_utils.failf "Invalid output JSON: %a" (Yojson.Basic.pretty_print ?std:None) json
