open! Astring
open! Tyxml.Html

type t = {
  name : string;
  state_repo : Uri.t option;
  can_read : CI_ACL.t;
  can_build : CI_ACL.t;
}

type page = user:string option -> [`Html] Tyxml.Html.elt

module Error = struct
  type t = string
  let no_state_repo = "no-state-repo"
  let permission_denied = "permission-denied"

  let uri_path id = "/error/" ^ id
  let uri id = Uri.of_string (uri_path id)
end

let config ?(name="datakit-ci") ?state_repo ~can_read ~can_build () = { name; state_repo; can_read; can_build }

let state_repo_url t fmt =
  fmt |> Fmt.kstrf @@ fun path ->
  match t.state_repo with
  | Some base -> Fmt.strf "%a/%s" Uri.pp_hum base path
  | None -> Error.(uri_path no_state_repo)

let log_commit_url t commit =
  state_repo_url t "commit/%s" commit

let log_branch_history_url t branch =
  state_repo_url t "commits/%s/log" branch

let log_branch_results_url t branch =
  state_repo_url t "tree/%s" branch

let gh_target_url = function
  | `PR pr ->
    let { CI_projectID.user; project } = CI_github_hooks.PR.project pr in
    let id = CI_github_hooks.PR.id pr in
    Printf.sprintf "https://github.com/%s/%s/pull/%d" user project id
  | `Ref r ->
    let { CI_projectID.user; project } = CI_github_hooks.Ref.project r in
    let id = CI_github_hooks.Ref.name r in
    match Datakit_path.unwrap id with
    | "tags" :: id -> Fmt.strf "https://github.com/%s/%s/releases/tag/%s" user project (String.concat ~sep:"/" id)
    | _            -> Fmt.strf "https://github.com/%s/%s/tree/%a" user project Datakit_path.pp id

let metadata_url t = function
  | `PR pr ->
    let { CI_projectID.user; project } = CI_github_hooks.PR.project pr in
    let id = CI_github_hooks.PR.id pr in
    state_repo_url t "commits/github-metadata/%s/%s/pr/%d" user project id
  | `Ref r ->
    let { CI_projectID.user; project } = CI_github_hooks.Ref.project r in
    let id = CI_github_hooks.Ref.name r in
    state_repo_url t "commits/github-metadata/%s/%s/ref/%a" user project
      Datakit_path.pp id

let commit_history_url t target =
  let { CI_projectID.user; project }, commit =
    match target with
    | `PR pr -> CI_github_hooks.PR.project pr, CI_github_hooks.PR.head pr
    | `Ref r -> CI_github_hooks.Ref.project r, CI_github_hooks.Ref.head r
  in
  let hash = CI_github_hooks.Commit.hash commit in
  state_repo_url t "commits/github-metadata/%s/%s/commit/%s" user project hash

let commit_url ~project commit =
  let { CI_projectID.user; project } = project in
  Printf.sprintf "https://github.com/%s/%s/commit/%s" user project commit

let pr_url { CI_projectID.user; project} pr =
  Printf.sprintf "/pr/%s/%s/%d" user project pr

let escape_ref path =
  Uri.pct_encode ~scheme:"http" (String.concat ~sep:"/" (Datakit_path.unwrap path))

let unescape_ref s =
  Uri.pct_decode s |> Datakit_path.of_string_exn

let ref_url { CI_projectID.user; project} r =
  Printf.sprintf "/ref/%s/%s/%s" user project (escape_ref r)

let tag_map f map =
  Datakit_path.Map.fold (fun key value acc ->
      match Datakit_path.unwrap key with
      | "tags" :: _ -> [f key value] @ acc
      | _ -> acc
    ) map []

let branch_map f map =
  Datakit_path.Map.fold (fun key value acc ->
      match Datakit_path.unwrap key with
      | "heads" :: _ -> [f key value] @ acc
      | _ -> acc
    ) map []

let int_map f map =
  CI_utils.IntMap.fold (fun key value acc ->
      [f key value] @ acc
    ) map []

let dash_map f map targets =
  Datakit_path.Map.fold (fun key value acc ->
      match CI_target.ID_Set.mem (`Ref key) targets with
      | true -> [f key value] @ acc
      | false -> acc
    ) map []

let status state =
  let colour, icon, status =
    match state.CI_state.status with
    | `Pending -> "label-warning", "glyphicon-hourglass", "Pending"
    | `Success -> "label-success", "glyphicon-ok","Success"
    | `Error -> "label-danger", "glyphicon-warning-sign", "Error"
    | `Failure -> "label-danger", "glyphicon-remove", "Failure"
  in
  span ~a:[a_class ["label"; colour;]] [span ~a:[a_class ["glyphicon"; icon]] []; pcdata status]

let status_list jobs =
  table ~a:[a_class ["ci-status-list"]] [
    tr (
      jobs |> List.map (fun job ->
          let state = CI_engine.state job in
          let cl, icon, status =
            match state.CI_state.status with
            | `Pending -> "label-warning", "glyphicon-hourglass", "pending"
            | `Success -> "label-success", "glyphicon-ok","success"
            | `Error -> "label-danger", "glyphicon-warning-sign", "error"
            | `Failure -> "label-danger", "glyphicon-remove", "failure"
          in
          let tooltip = Printf.sprintf "%s : %s" (CI_engine.job_name job) status in
          td ~a:[a_class [cl]; a_title tooltip] [span ~a:[a_class ["glyphicon"; icon]] []]
        )
    )
  ]

let summarise jobs =
  let states = List.map (fun j -> CI_engine.job_name j, CI_engine.state j) jobs in
  let combine status states =
    let results = ref String.Map.empty in
    states |> List.iter (fun (name, state) ->
        let descr = state.CI_state.descr in
        let old_names = String.Map.find descr !results |> CI_utils.default [] in
        results := String.Map.add descr (name :: old_names) !results
      );
    let results = String.Map.bindings !results in
    let pp_group f (descr, g) = Fmt.pf f "%s (%a)" descr (Fmt.(list ~sep:(const string ", ") Fmt.string)) (List.rev g) in
    let descr = Fmt.strf "%a" Fmt.(list ~sep:(const string "; ") pp_group) results in
    { CI_state.status; descr; logs = CI_result.Step_log.Empty }
  in
  let pending, states = List.partition (fun (_, x) -> x.CI_state.status = `Pending) states in
  if pending <> [] then combine `Pending pending
  else (
    let failed, states = List.partition (fun (_, x) -> x.CI_state.status = `Failure) states in
    if failed <> [] then combine `Failure failed
    else combine `Success states
  )

let dashboard_widget id ref =
  let state = CI_engine.jobs ref |> summarise in
  let cls, icon, status, comment =
    match state.CI_state.status with
    | `Pending -> "dashboard-pending", "glyphicon-hourglass", "Pending", "... WAITING ..."
    | `Success -> "dashboard-success", "glyphicon-ok", "Succeeding", "YAY! The build is fine... Nothing to see here..."
    | `Error -> "dashboard-error", "glyphicon-warning-sign", "Erroring", "OH NO! Something has gone terribly wrong"
    | `Failure -> "dashboard-failure", "glyphicon-remove", "Failing", "SOUND THE ALARM!!! The build has been broken!"
  in
  let title =
    match Datakit_path.unwrap id with
    | ("heads" | "tags") :: tl -> tl
    | x -> x
  in
  div ~a:[a_class ["col-md-4"; "text-center"; "dashboard"; cls]] [
    h2 ~a:[a_id "title"] [ pcdata (String.concat ~sep:"/" title) ];
    h2 ~a:[a_id "icon"] [ span ~a:[a_class["glyphicon"; icon]] [] ];
    h2 ~a:[a_id "status"] [ pcdata status ];
    small [ pcdata comment ];
  ]

let ref_job ~project id ref =
  let jobs = CI_engine.jobs ref in
  let summary = summarise jobs in
  tr [
    td [a ~a:[a_href (ref_url project id)] [pcdata (Fmt.to_to_string Datakit_path.pp id)]];
    td [status_list jobs];
    td [pcdata summary.CI_state.descr];
  ]

let pr_job ~project id open_pr =
  let jobs = CI_engine.jobs open_pr in
  let summary = summarise jobs in
  tr [
    td [a ~a:[a_href (pr_url project id)] [pcdata (string_of_int id)]];
    td [pcdata (CI_engine.title open_pr)];
    td [status_list jobs];
    td [pcdata summary.CI_state.descr];
  ]

let heading x = th [pcdata x]


module Nav = struct
  type t =
    | Home
    | PRs
    | Branches
    | Tags

  let to_string = function
    | Home -> "Home"
    | PRs -> "PRs"
    | Branches -> "Branches"
    | Tags -> "Tags"
end

let build_navbar active =
  let item name href =
    let cl = if name = active then ["active"] else [] in
    li ~a:[a_class cl] [a ~a:[a_href href] [pcdata (Nav.to_string name)]];
  in
  Nav.[
    item Home "/";
    item PRs "/pr";
    item Branches "/branch";
    item Tags "/tag";
  ]

let page ?logs page_title active children t ~user =
  let navbar = build_navbar active in
  let user = user |> CI_utils.default "not logged in" in
  let nav_header =
    nav ~a:[a_class["navbar";"navbar-inverse";"navbar-fixed-top"]] [
      div ~a:[a_class["container"]] [
        div ~a:[a_class["navbar-header"]] [
          button ~a:[a_user_data "toggle" "collapse"; a_user_data "target" "#navbar"; a_class ["navbar-toggle"]; a_button_type `Button;] [

            span ~a:[a_class["sr-only"]] [pcdata "Toggle Navigation"];
            span ~a:[a_class["icon-bar"]] [];
            span ~a:[a_class["icon-bar"]] [];
            span ~a:[a_class["icon-bar"]] [];

          ];
          a ~a:[a_class["navbar-brand"]; a_href "/"] [pcdata t.name];
        ];
        div ~a:[a_id "navbar"; a_class["collapse"; "navbar-collapse"]] [
          ul ~a:[a_class ["nav"; "navbar-nav"]] navbar;
          ul ~a:[a_class ["nav"; "navbar-nav"; "navbar-right"]] [
            li [a ~a:[a_href "/user/profile"] [pcdata user]]
          ];
        ];
      ];
    ]
  in
  let content =
    [
      div ~a:[a_class ["container"]] [
        div ~a:[a_class ["content"]] children;
      ];
    ]
  in
  let body =
    match logs with
    | Some init ->
      let attrs =
        match init with
        | None -> []
        | Some init -> [a_src init]
      in
      body ~a:[a_class ["split-page"]] [
        nav_header;
        div ~a:[a_class ["upper"]] content;
        iframe ~a:(a_id "iframe_log" :: a_class ["log"] :: a_onload "highlight_log()" :: a_name "iframe_log" :: attrs) [];
      ]
    | None ->
      body (nav_header :: content)
  in
  html
    (head (title (pcdata page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        meta ~a:[a_http_equiv "X-UA-Compatible"; a_content "IE=edge"] ();
        meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1"] ();
        link ~rel:[`Icon] ~a:[a_mime_type "image/png"] ~href:"/images/favicon.png" ();
        script ~a:[a_mime_type "text/javascript"; a_src "/js/ci.js"] (pcdata "");
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
      ])
    body

let opt_warning ci =
  let dk = CI_engine.dk ci in
  match Lwt.state dk with
  | Lwt.Return _ -> []
  | Lwt.Sleep -> [div ~a:[a_class ["warning"]] [pcdata "Connecting to DataKit..."]]
  | Lwt.Fail ex ->
    let msg = Fmt.strf "DataKit connection is down: %s" (Printexc.to_string ex) in
    [div ~a:[a_class ["warning"]] [pcdata msg]]

let pr_table (id, (prs, _)) =
  div [
    h2 [pcdata (Fmt.strf "PR status for %a" CI_projectID.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "PR"; heading "Title"; heading "State"; heading "Details"] ::
      (int_map (pr_job ~project:id) prs)
    );
  ]

let branch_table (id, (_, refs)) =
  div [
    h2 [pcdata (Fmt.strf "Branches for %a" CI_projectID.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "Ref"; heading "State"; heading "Details"] ::
      (branch_map (ref_job ~project:id) refs)
    );
  ]

let tag_table (id, (_, refs)) =
  div [
    h2 [pcdata (Fmt.strf "Tags for %a" CI_projectID.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "Ref"; heading "State"; heading "Details"] ::
      (tag_map (ref_job ~project:id) refs)
    );
  ]

let rec make_pairs pairs = function
  | [] -> List.rev pairs
  | [x] -> List.rev ((Some x, None)::pairs)
  | x::y::rest -> make_pairs ((Some x, Some y)::pairs) rest

let dashboard_row acc x =
  let (l,r) = x in
  let col1 = match l with
    | None -> div ~a:[a_class["col-md-4"]] [];
    | Some l -> l;
  in
  let col2 = match r with
    | None -> div ~a:[a_class["col-md-4"]] [];
    | Some r -> r;
  in
  div ~a:[a_class["row"]]
    [
      div ~a:[a_class["col-md-1"]] [];
      col1;
      div ~a:[a_class["col-md-2"]] [];
      col2;
      div ~a:[a_class["col-md-1"]] [];
    ]
  :: acc

let dashboard_table _id (refs, targets) acc =
  let widgets = dash_map dashboard_widget refs targets in
  let widget_pairs = make_pairs [] widgets in
  List.fold_left (fun acc x -> dashboard_row acc x) [] widget_pairs @ acc

let html_of_user ~csrf_token (reason, log) =
  let cancel_attrs, branch =
    match log with
    | Some log when CI_live_log.can_cancel log -> [], CI_live_log.branch log
    | _ -> [a_disabled ()], ""
  in
  let branch = Uri.pct_encode ~scheme:"http" branch in
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/cancel/%s?%s" branch (Uri.encoded_of_query query) in
  [
    br ();
    form ~a:[a_class ["cancel"]; a_action action; a_method `Post] [
      button ~a:(a_class ["btn"; "btn-default"] :: a_button_type `Submit :: cancel_attrs) [
        span ~a:[a_class ["glyphicon"; "glyphicon-remove"]] []; pcdata "Cancel"
      ];
    ];
    pcdata reason;
  ]

let resource_pools ~csrf_token =
  let items =
    let open CI_monitored_pool in
    String.Map.bindings (pools ()) |> List.map (fun (name, pool) ->
        let used = Fmt.strf "%d / %d" (active pool) (capacity pool) in
        let uses = users pool |> List.map (html_of_user ~csrf_token) |> List.concat in
        tr [th [pcdata name]; td (pcdata used :: uses)];
      )
  in
  table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
    tr [th [pcdata "Name"]; th [pcdata "Utilisation"]] ::
    items
  )

let login_page ?github ~csrf_token t ~user =
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/auth/login?%s" (Uri.encoded_of_query query) in
  let field descr ty name =
    let id = "field-" ^ name in
    div ~a:[a_class ["form-group"]] [
      label ~a:[a_label_for id] [pcdata descr];
      input ~a:[a_class ["form-control"]; a_id id; a_input_type ty; a_name name] ()
    ]
  in
  let github_login =
    match github with
    | None ->
      p [pcdata "(configure webauth to allow GitHub logins)"]
    | Some github ->
      p [
        a ~a:[a_href (Uri.to_string github)] [pcdata "Log in with GitHub"];
      ]
  in
  page "Login" Nav.Home ~user [
    h2 [pcdata "Login"];
    form ~a:[a_class ["login-form"]; a_action action; a_method `Post; a_enctype "multipart/form-data"] [
      field "Username" `Text "user";
      field "Password" `Password "password";
      div [button ~a:[a_class ["btn"; "btn-primary"]; a_button_type `Submit] [pcdata "Log in"]];
    ];
    github_login;
  ] t

let user_page ~csrf_token =
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/auth/logout?%s" (Uri.encoded_of_query query) in
  page "Profile" Nav.Home [
    form ~a:[a_class ["logout-form"]; a_action action; a_method `Post] [
      button ~a:[a_class ["btn"; "btn-default"]; a_button_type `Submit] [
        pcdata "Log out"
      ]
    ]
  ]

let main_page ~csrf_token ~ci ~dashboards =
  let projects = CI_engine.targets ci in
  let title_of_project (id, _) = Fmt.to_to_string CI_projectID.pp id in
  let title = "CI: " ^ (CI_projectID.Map.bindings projects |> List.map title_of_project |> String.concat ~sep:" / ") in
  let dashboard_widgets =
    let combined =
      CI_projectID.Map.merge (fun _ project_state dash_config ->
          match project_state, dash_config with
          | Some (_prs, refs), Some y -> Some (refs, y)
          | _ -> None
        ) projects dashboards
    in
    CI_projectID.Map.fold dashboard_table combined []
  in
  page title Nav.Home @@ opt_warning ci @ dashboard_widgets @ [
      h2 [pcdata "Resource pools"];
      resource_pools ~csrf_token;
    ]

let prs_page ~ci =
  let projects = CI_engine.targets ci |> CI_projectID.Map.bindings in
  let sections = List.map pr_table projects in
  let title_of_project (id, _) = Fmt.to_to_string CI_projectID.pp id in
  let title = "CI: " ^ (List.map title_of_project projects |> String.concat ~sep:" / ") in
  page title Nav.PRs @@ opt_warning ci @ sections

let branches_page ~ci =
  let projects = CI_engine.targets ci |> CI_projectID.Map.bindings in
  let sections = List.map branch_table projects in
  let title_of_project (id, _) = Fmt.to_to_string CI_projectID.pp id in
  let title = "CI: " ^ (List.map title_of_project projects |> String.concat ~sep:" / ") in
  page title Nav.Branches @@ opt_warning ci @ sections

let tags_page ~ci =
  let projects = CI_engine.targets ci |> CI_projectID.Map.bindings in
  let sections = List.map tag_table projects in
  let title_of_project (id, _) = Fmt.to_to_string CI_projectID.pp id in
  let title = "CI: " ^ (List.map title_of_project projects |> String.concat ~sep:" / ") in
  page title Nav.Tags @@ opt_warning ci @ sections

let history_button url =
  a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href url;] [span ~a:[a_class["glyphicon"; "glyphicon-time"]] []; pcdata "History"]

let log_button_group history log_url =
  div ~a:[a_class["btn-group";"pull-right"]] [
    history;
    a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href log_url;] [span ~a:[a_class["glyphicon"; "glyphicon-book"]] []; pcdata "Artefacts"];
  ]

let encode = Uri.pct_encode ~scheme:"http"

module LogScore : sig
  type t
  type log = [`Live of CI_live_log.t | `Saved of CI_result.Step_log.saved]

  val create : unit -> t
  val update : t -> log -> unit
  val best : t -> log option
end = struct
  type score = int
  type log = [`Live of CI_live_log.t | `Saved of CI_result.Step_log.saved]
  type t = (score * log) option ref

  let ok = 1
  let pending = 2
  let failed = 3

  let create () = ref None

  let update (best:t) (x:log) =
    let new_score =
      match x with
      | `Live _ -> pending
      | `Saved {CI_result.Step_log.failed = true; _} -> failed
      | `Saved _ -> ok
    in
    match !best with
    | None -> best := Some (new_score, x)
    | Some (score, _) when score < new_score -> best := Some (new_score, x)
    | _ -> ()

  let best t =
    match !t with
    | None -> None
    | Some (_, x) -> Some x
end

let logs_frame_link = function
  | `Live live_log -> Printf.sprintf "/log/live/%s" (encode (CI_live_log.branch live_log))
  | `Saved {CI_result.Step_log.branch; commit; _} -> Printf.sprintf "/log/saved/%s/%s" (encode branch) (encode commit)

let score_logs ~best job =
  let open CI_result.Step_log in
  let rec aux = function
    | Empty -> ()
    | Live live_log -> LogScore.update best (`Live live_log);
    | Saved saved -> LogScore.update best (`Saved saved);
    | Pair (a, b) -> aux a; aux b
  in
  aux (CI_engine.state job).CI_state.logs

let logs ~csrf_token ~page_url ~selected logs =
  let open CI_result.Step_log in
  let seen = ref String.Set.empty in
  let selected_branch =
    match selected with
    | Some (`Live x) -> Some (CI_live_log.branch x)
    | Some (`Saved x) -> Some x.CI_result.Step_log.branch
    | None -> None
  in
  let log_link ~branch ~title log =
    let cl =
      if Some branch = selected_branch then ["log-link"; "selected-log"]
      else ["log-link"]
    in
    let href = logs_frame_link log in
    span [
      pcdata "[ ";
      a ~a:[a_href href; a_target "iframe_log"; a_class cl] [pcdata "logs"];
      pcdata " ] ";
      pcdata title;
    ]
  in
  let rec aux = function
    | Empty -> []
    | Live log when String.Set.mem (CI_live_log.branch log) !seen -> []
    | Saved { branch; _ } when String.Set.mem branch !seen -> []
    | Live live_log ->
      let branch = CI_live_log.branch live_log in
      seen := String.Set.add branch !seen;
      let title = CI_live_log.title live_log in
      [
        form [
          button ~a:[a_class ["btn"; "btn-default"; "btn-xs"; "rebuild"]; a_button_type `Submit; a_disabled ()] [
            span ~a:[a_class ["glyphicon"; "glyphicon-refresh"; "pull-left"]] []; pcdata "Rebuild"];
          log_link ~branch ~title (`Live live_log);
        ];
      ]
    | Saved ({CI_result.Step_log.commit = _; branch; title; rebuild = _; failed} as saved) ->
      seen := String.Set.add branch !seen;
      let query = [
        "CSRFToken", [csrf_token];
        "redirect", [page_url];
      ] in
      let action = Printf.sprintf "/log/rebuild/%s?%s" branch (Uri.encoded_of_query query) in
      let status = if failed then "failed" else "passed" in
      [
        form ~a:[a_action action; a_method `Post] [
          button ~a:[a_class ["btn"; "btn-default"; "btn-xs"; "rebuild"; status]; a_button_type `Submit] [
            span ~a:[a_class ["glyphicon"; "glyphicon-refresh"; "pull-left"]] []; pcdata "Rebuild"];
          log_link ~branch ~title (`Saved saved);
        ]
      ]
    | Pair (a, b) -> aux a @ aux b
  in
  aux logs

let job_row ~csrf_token ~page_url ~best_log job =
  let state = CI_engine.state job in
  let job_name = CI_engine.job_name job in
  tr [
    th [pcdata job_name];
    td [status state];
    td (
      p [pcdata state.CI_state.descr] ::
      logs ~csrf_token ~page_url ~selected:best_log (CI_engine.state job).CI_state.logs
    );
  ]

let target_title = function
  | `PR pr -> Printf.sprintf "PR %d" (CI_github_hooks.PR.id pr)
  | `Ref r -> Fmt.strf "Ref %a" Datakit_path.pp (CI_github_hooks.Ref.name r)

let target_commit = function
  | `PR pr -> CI_github_hooks.Commit.hash (CI_github_hooks.PR.head pr)
  | `Ref r -> CI_github_hooks.Commit.hash (CI_github_hooks.Ref.head r)

let target_project = function
  | `PR pr -> CI_github_hooks.PR.project pr
  | `Ref r -> CI_github_hooks.Ref.project r

let target_page_url target =
  let project = target_project target in
  match target with
  | `PR pr -> pr_url project (CI_github_hooks.PR.id pr)
  | `Ref r -> ref_url project (CI_github_hooks.Ref.name r)

let target_page ~csrf_token ~target jobs t =
  let target = CI_engine.git_target target in
  let title = target_title target in
  let project = target_project target in
  let commit = target_commit target in
  let page_url = target_page_url target in
  let best_log =
    let best = LogScore.create () in
    List.iter (score_logs ~best) jobs;
    LogScore.best best
  in
  let state_summary = [
    table ~a:[a_class ["table"; "table-bordered"; "results"]] (List.map (job_row ~csrf_token ~page_url ~best_log) jobs)
  ] in
  let nav =
    match target with
    | `PR _ -> Nav.PRs
    | `Ref r ->
      match CI_github_hooks.Ref.name r |> Datakit_path.unwrap with
      | "heads" :: _ -> Nav.Branches
      | "tags" :: _ -> Nav.Tags
      | _ -> assert false
  in
  let logs =
    match best_log with
    | None -> None
    | Some best -> Some (logs_frame_link best)
  in
  page ~logs title nav (
    p [
      a ~a:[a_href (gh_target_url target)] [pcdata title];
      pcdata " has head commit ";
      a ~a:[a_href (commit_url ~project commit)] [pcdata commit];
      pcdata " [ ";
      a ~a:[a_href (metadata_url t target)] [pcdata "head history"];
      pcdata " ]";
      pcdata " [ ";
      a ~a:[a_href (commit_history_url t target)] [pcdata "status history for this head"];
      pcdata " ]";
    ]
    :: state_summary
  ) t

let plain_page ~page_title =
  html
    (head (title (pcdata page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
      ])

let iframe_page ~page_title =
  html
    (head (title (pcdata page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
        base ~a:[a_target "_parent"] ();
      ])

let plain_error msg _t ~user:_ =
  plain_page ~page_title:"Error" (body [pcdata msg])

let highlighted_log data =
  Ansiparse.Concrete.parse_str data
  |> Ansiparse.Abstract.parse
  |> Ansiparse.Html.of_tree

let live_log_frame ~branch ~live_log ~have_history t ~user:_ =
  let buttons =
    if have_history then [history_button (log_branch_history_url t branch)]
    else []
  in
  let page_title = Fmt.strf "Logging to branch %s" branch in
  iframe_page ~page_title
    (body ~a:[a_class ["log"]] [
        div ~a:[a_class ["row"]] [
          div ~a:[a_class ["col-md-9"]] [
            p [pcdata "Still running..."];
          ];
          div ~a:[a_class ["col-md-3"]] [
            div ~a:[a_class["btn-group";"pull-right"]] buttons
          ]
        ];
        highlighted_log (CI_live_log.contents live_log);
      ]
    )

let saved_log_frame ~commit ~branch ~log_data t ~user:_ =
  let page_title = Fmt.strf "Log from commit %s" commit in
  let history = history_button (log_branch_history_url t branch) in
  let log_url = log_branch_results_url t branch in
  let commit_url = log_commit_url t commit in
  iframe_page ~page_title
    (body ~a:[a_class ["log"]] [
        div ~a:[a_class ["row"]] [
          div ~a:[a_class ["col-md-9"]] [
            p [pcdata "Loaded from commit "; a ~a:[a_href commit_url] [pcdata commit]]
          ];
          div ~a:[a_class ["col-md-3"]] [
            log_button_group history log_url;
          ];
        ];
        highlighted_log (Cstruct.to_string log_data);
      ]
    )

let error_page id =
  page "Error" Nav.Home (
    if id = Error.no_state_repo then
      [
        p [pcdata "No web mirror of the state repository has been configured, so can't link to it."];
        p [pcdata "Configure DataKit to push to a GitHub repository and then pass the repository's URL using ";
           code [pcdata "Web.config ~state_repo"];
           pcdata "."
          ];
      ]
    else if id = Error.permission_denied then
      [
        p [pcdata "Permission denied"];
      ]
    else
      [
        p [pcdata (Printf.sprintf "Unknown error code %S" id)]
      ]
  )
