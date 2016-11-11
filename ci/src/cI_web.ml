open! Astring
open Lwt.Infix
open CI_utils

module Wm = CI_web_utils.Wm
module Rd = Webmachine.Rd

type t = {
  ci : CI_engine.t;
  logs : CI_live_log.manager;
  server : CI_web_utils.server;
  dashboards : CI_target.ID_Set.t CI_projectID.Map.t;
}

class virtual http_page t = object(self)
  inherit CI_web_utils.protected_page t.server

  method virtual private render : (CI_web_templates.t -> CI_web_templates.page, Cohttp_lwt_body.t) Wm.op

  method content_types_provided rd =
    Wm.continue [
      "text/html" , self#to_html;
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method private to_html rd =
    self#render rd >>= fun (resp, rd) ->
    match resp with
    | Wm.Error _ as e -> Lwt.return (e, rd)
    | Wm.Ok html ->
      let user = self#authenticated_user in
      let body = Fmt.to_to_string (Tyxml.Html.pp ()) (html ~user (CI_web_utils.web_config t.server)) in
      Wm.continue (`String body) rd
end

class user_page t = object(self)
  inherit http_page t

  method private required_roles = [`LoggedIn]

  method private render rd =
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.user_page ~csrf_token) rd
end

class main t = object(self)
  inherit http_page t

  method private required_roles = [`Reader]

  method private render rd =
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.main_page ~csrf_token ~ci:t.ci ~dashboards:t.dashboards) rd
end

class error t = object
  inherit http_page t

  method private required_roles = []

  method private render rd =
    let id = Rd.lookup_path_info_exn "id" rd in
    Wm.continue (CI_web_templates.error_page id) rd
end

class pr_list t = object
  inherit http_page t

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.prs_page ~ci:t.ci) rd
end

class branch_list t = object
  inherit http_page t

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.branches_page ~ci:t.ci) rd
end

class tag_list t = object
  inherit http_page t

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.tags_page ~ci:t.ci) rd
end

class pr_page t = object(self)
  inherit http_page t

  method private required_roles = [`Reader]

  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let project = Rd.lookup_path_info_exn "project" rd in
    let id = Rd.lookup_path_info_exn "id" rd in
    let id = int_of_string id in
    let project = CI_projectID.v ~user ~project in
    let projects = CI_engine.targets t.ci in
    match CI_projectID.Map.find project projects with
    | None -> Wm.respond 404 rd ~body:(`String "No such project")
    | Some (prs, _) ->
      match IntMap.find id prs with
      | None -> Wm.respond 404 rd ~body:(`String "No such open PR")
      | Some target ->
        let jobs = CI_engine.jobs target in
        self#session rd >>= fun session_data ->
        let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
        Wm.continue (CI_web_templates.target_page ~csrf_token ~target jobs) rd
end

class ref_page t = object(self)
  inherit http_page t

  method private required_roles = [`Reader]

  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let project = Rd.lookup_path_info_exn "project" rd in
    let id = Rd.lookup_path_info_exn "id" rd in
    let id = CI_web_templates.unescape_ref id in
    let project = CI_projectID.v ~user ~project in
    let projects = CI_engine.targets t.ci in
    match CI_projectID.Map.find project projects with
    | None -> Wm.respond 404 rd ~body:(`String "No such project")
    | Some (_, refs) ->
      match Datakit_path.Map.find id refs with
      | exception Not_found -> Wm.respond 404 rd ~body:(`String "No such ref")
      | target ->
        let jobs = CI_engine.jobs target in
        self#session rd >>= fun session_data ->
        let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
        Wm.continue (CI_web_templates.target_page ~csrf_token ~target jobs) rd
end

class live_log_page t = object
  inherit http_page t

  method private required_roles = [`Reader]

  method private render rd =
    let branch = Rd.lookup_path_info_exn "branch" rd in
    match CI_live_log.lookup t.logs ~branch with
    | None ->
      (* todo: find out what commit it turned into and serve that instead *)
      Wm.continue (CI_web_templates.plain_error "This log is no longer building") rd
    | Some live_log ->
      CI_engine.dk t.ci >>= fun dk ->
      DK.branch dk branch >>*= fun b ->
      DK.Branch.head b >>*= fun head ->
      let have_history = head <> None in
      Wm.continue (CI_web_templates.live_log_frame ~branch ~live_log ~have_history) rd
end

class saved_log_page t = object
  inherit http_page t

  method private required_roles = [`Reader]

  method private render rd =
    let branch = Rd.lookup_path_info_exn "branch" rd in
    let commit = Rd.lookup_path_info_exn "commit" rd in
    CI_engine.dk t.ci >>= fun dk ->
    let tree = DK.Commit.tree (DK.commit dk commit) in
    DK.Tree.read_file tree CI_cache.Path.log >>= function
    | Error (`Msg e) -> Wm.continue (CI_web_templates.plain_error e) rd
    | Ok log_data ->
      Wm.continue (CI_web_templates.saved_log_frame ~commit ~branch ~log_data) rd
end

class rebuild t = object
  inherit CI_web_utils.post_page t.server

  method private required_roles = [`Builder]

  method! private process_post rd =
    let branch_name = Rd.lookup_path_info_exn "branch" rd in
    match Uri.get_query_param rd.Rd.uri "redirect" with
    | None -> Wm.respond ~body:(`String "Missing redirect") 400 rd
    | Some redirect ->
      CI_engine.rebuild t.ci ~branch_name >>= fun () ->
      Wm.continue true (Rd.redirect redirect rd)
end

class cancel t = object
  inherit CI_web_utils.post_page t.server

  method private required_roles = [`Builder]

  method! private process_post rd =
    let branch = Rd.lookup_path_info_exn "branch" rd in
    match CI_live_log.lookup ~branch t.logs with
    | None ->
      let body = Fmt.strf "Branch %S is not currently building" branch in
      Wm.respond ~body:(`String body) 404 rd
    | Some log ->
      CI_live_log.cancel log >>= function
      | Ok () -> Wm.continue true (Rd.redirect "/" rd)
      | Error body -> Wm.respond ~body:(`String body) 404 rd
end

let mime_type uri =
  match String.take ~sat:((<>) '.') ~rev:true (Uri.path uri) with
  | "css"   -> Some "text/css"
  | "js"    -> Some "text/javascript"
  | "eot"   -> Some "application/vnd.ms-fontobject"
  | "svg"   -> Some "image/svg+xml"
  | "ttf"   -> Some "application/x-font-ttf"
  | "woff"  -> Some "application/font-woff"
  | "woff2" -> Some "font/woff2"
  | "png"   -> Some "image/png"
  | _       -> None

let rec matches_acl ~auth ~user acl =
  match acl, user with
  | `Everyone, _ -> Lwt.return true
  | `Username required, Some actual -> Lwt.return (required = actual)
  | `Github_org org, Some user -> CI_web_utils.Auth.github_orgs auth ~user >|= List.mem org
  | `Can_read project, Some user -> CI_web_utils.Auth.can_read_github auth ~user project
  | `Any xs, _ -> Lwt_list.exists_s (matches_acl ~auth ~user) xs
  | (`Username _ | `Github_org _ | `Can_read _), None -> Lwt.return false

let routes ~config ~logs ~ci ~auth ~dashboards =
  let has_role r ~user =
    match r with
    | `Reader -> matches_acl config.CI_web_templates.can_read ~auth ~user
    | `Builder -> matches_acl config.CI_web_templates.can_build ~auth ~user
    | `LoggedIn -> Lwt.return (user <> None)
  in
  let server = CI_web_utils.server ~auth ~web_config:config ~has_role in
  let t = { logs; ci; server; dashboards } in
  [
    (* Auth *)
    ("/auth/login",     fun () -> new CI_web_utils.login_page t.server);
    ("/auth/logout",    fun () -> new CI_web_utils.logout_page t.server);
    ("/auth/github-callback",    fun () -> new CI_web_utils.github_callback t.server);
    ("/user/profile",   fun () -> new user_page t);
    (* Overview pages *)
    ("/",               fun () -> new main t);
    ("pr",              fun () -> new pr_list t);
    ("branch",          fun () -> new branch_list t);
    ("tag",             fun () -> new tag_list t);
    (* Individual targets *)
    ("pr/:user/:project/:id",                   fun () -> new pr_page t);
    ("ref/:user/:project/:id",                  fun () -> new ref_page t);
    (* Logs *)
    ("log/live/:branch",                        fun () -> new live_log_page t);
    ("log/saved/:branch/:commit",               fun () -> new saved_log_page t);
    ("log/rebuild/:branch",                     fun () -> new rebuild t);
    ("cancel/:branch",                          fun () -> new cancel t);
    (* Errors *)
    ("error/:id",       fun () -> new error t);
    (* Static resources *)
    (":dir/:name",      fun () -> new CI_web_utils.static_crunch ~mime_type CI_static.read);
  ]
