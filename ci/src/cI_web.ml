open Datakit_github
open! Astring
open Lwt.Infix
open CI_utils
open CI_utils.Infix

let ( / ) = Filename.concat

let acme_web_root = "/var/run/datakit/"
let acme_challenge_dir = acme_web_root / ".well-known/acme-challenge"
(* Any token files placed here (e.g. by certbot) will be served at ".well-known/acme-challenge/..." *)

module Wm = CI_web_utils.Wm
module Rd = Webmachine.Rd

type t = {
  ci : CI_engine.t;
  logs : CI_live_log.manager;
  server : CI_web_utils.server;
  dashboards : CI_target.Set.t Repo.Map.t;
  domains : string list;                (* Domains to pass to certbot *)
}

class user_page t = object(self)
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`LoggedIn]

  method private render rd =
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.user_page ~csrf_token) rd
end

class main t = object(self)
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd =
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.main_page ~csrf_token ~ci:t.ci ~dashboards:t.dashboards) rd
end

class error t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = []

  method private render rd =
    let id = Rd.lookup_path_info_exn "id" rd in
    Wm.continue (CI_web_templates.error_page id) rd
end

let check_metrics_token server provided =
  match String.cut ~sep:" " provided with
  | Some (typ, provided) when String.Ascii.lowercase typ = "bearer" ->
    begin match (CI_web_utils.web_config server).CI_web_templates.metrics_token with
      | None -> false
      | Some (`SHA256 expected_hash) ->
        let user_hash = (Nocrypto.Hash.SHA256.digest (Cstruct.of_string provided)) in
        if Cstruct.equal expected_hash user_hash then true
        else (
          Log.info (fun f ->
              f "Bad /metrics token. Expected:@\n%aGot:@\n%a"
                Cstruct.hexdump_pp expected_hash
                Cstruct.hexdump_pp user_hash
            );
          false
        )
    end
  | _ ->
    Log.info (fun f -> f "Bad token %S" provided);
    false

class metrics t = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided rd =
    Wm.continue [
      "text/plain; version=0.0.4" , self#to_plain;
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method! is_authorized rd =
    match Cohttp.Header.get_authorization rd.Rd.req_headers with
    | Some (`Other token) when check_metrics_token t.server token -> Wm.continue `Authorized rd
    | _ ->
      Wm.respond ~body:(`String "Bad token") 403 rd

  method private to_plain rd =
    let data = CI_prometheus.(CollectorRegistry.collect CollectorRegistry.default) in
    let body = Fmt.to_to_string CI_prometheus.TextFormat_0_0_4.output data in
    Wm.continue (`String body) rd
end

class pr_list t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.prs_page ~ci:t.ci) rd
end

class branch_list t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.branches_page ~ci:t.ci) rd
end

class tag_list t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.tags_page ~ci:t.ci) rd
end

class pr_page t = object(self)
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let repo = Rd.lookup_path_info_exn "repo" rd in
    let id = Rd.lookup_path_info_exn "id" rd in
    let id = int_of_string id in
    let repo = Repo.v ~user ~repo in
    let prs = CI_engine.prs t.ci in
    match Repo.Map.find repo prs with
    | None  -> Wm.respond 404 rd ~body:(`String "No such project")
    | Some prs ->
      match PR.Index.find (repo, id) prs with
      | None -> Wm.respond 404 rd ~body:(`String "No such open PR")
      | Some target ->
        let jobs = CI_engine.jobs target in
        self#session rd >>= fun session_data ->
        let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
        Wm.continue (CI_web_templates.target_page ~csrf_token ~target jobs) rd
end

class ref_page t = object(self)
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let repo = Rd.lookup_path_info_exn "repo" rd in
    let id = CI_target.unescape_ref rd.Rd.dispatch_path in
    let repo = Repo.v ~user ~repo in
    let refs = CI_engine.refs t.ci in
    match Repo.Map.find repo refs with
    | None -> Wm.respond 404 rd ~body:(`String "No such project")
    | Some refs ->
      match Ref.Index.find (repo, id) refs with
      | None        -> Wm.respond 404 rd ~body:(`String "No such ref")
      | Some target ->
        let jobs = CI_engine.jobs target in
        self#session rd >>= fun session_data ->
        let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
        Wm.continue (CI_web_templates.target_page ~csrf_token ~target jobs) rd
end

class live_log_page t = object
  inherit CI_web_utils.html_page t.server

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
  inherit CI_web_utils.html_page t.server

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

let acme_challenge =
  let url_safe_b64 = Str.regexp "^[=-_A-Za-z0-9]+$" in
  let mime_type _ = Some "application/jose+json" in
  fun () -> new CI_web_utils.static ~valid:url_safe_b64 ~mime_type acme_challenge_dir

let run_certbot domains =
  let cmd = ["certbot"; "certonly";
              "--preferred-challenges"; "tls-sni";
              "--webroot"; acme_web_root;
            ] :: List.map (fun d -> ["-d"; d]) domains
            |> List.flatten
  in
  let log = Buffer.create 1024 in
  Lwt.catch
    (fun () -> CI_process.run ~output:(Buffer.add_string log) ("", Array.of_list cmd))
    (fun ex ->
       Buffer.add_string log ("\nError running certbot:\n" ^ Printexc.to_string ex);
       Lwt.return ()
    )
  >|= fun () ->
  Buffer.contents log

class update_cert t = object
  inherit [unit] CI_web_utils.form_page t.server

  method private required_roles = [`Admin]

  method private render ~csrf_token =
    CI_web_templates.update_cert_form ~csrf_token

  method private validate = CI_form.Validator.maybe ()

  method private process () rd =
    run_certbot t.domains >>= fun log ->
    Wm.continue true
      { rd with Rd.
             resp_body = Cohttp_lwt_body.of_string log;
             resp_headers = Cohttp.Header.add rd.Rd.resp_headers "Content-Type" "text/plain";
      }
end

let routes ~logs ~ci ~server ~dashboards ~domains =
  let t = { logs; ci; server; dashboards; domains } in
  [
    (* Auth *)
    ("auth/intro/:token", fun () -> new CI_web_utils.auth_intro t.server);
    ("auth/setup",     fun () -> new CI_web_utils.auth_setup t.server);
    ("auth/login",     fun () -> new CI_web_utils.login_page t.server);
    ("auth/logout",    fun () -> new CI_web_utils.logout_page t.server);
    ("auth/github-callback",    fun () -> new CI_web_utils.github_callback t.server);
    ("user/profile",   fun () -> new user_page t);
    (* Overview pages *)
    ("/",               fun () -> new main t);
    ("pr",              fun () -> new pr_list t);
    ("branch",          fun () -> new branch_list t);
    ("tag",             fun () -> new tag_list t);
    (* Individual targets *)
    (":user/:repo/pr/:id",     fun () -> new pr_page t);
    (":user/:repo/ref/*",      fun () -> new ref_page t);

    (* Logs *)
    ("log/live/:branch",                        fun () -> new live_log_page t);
    ("log/saved/:branch/:commit",               fun () -> new saved_log_page t);
    ("log/rebuild/:branch",                     fun () -> new rebuild t);
    ("cancel/:branch",                          fun () -> new cancel t);
    (* Admin *)
    ("config/update-certificate",               fun () -> new update_cert t);
    (* Errors *)
    ("error/:id",       fun () -> new error t);
    (* Reporting *)
    ("metrics",         fun () -> new metrics t);
    (* certbot *)
    (".well-known/acme-challenge/:name", acme_challenge);
    (* Static resources *)
    (":dir/:name",      fun () -> new CI_web_utils.static_crunch ~mime_type CI_static.read);
  ]
