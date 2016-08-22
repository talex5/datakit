open Astring
open Test_utils
open Lwt.Infix
open Datakit_github
open Datakit_path.Infix
open Result

module Conv = Conv(DK)

module Counter = struct
  type t = {
    mutable events    : int;
    mutable prs       : int;
    mutable status    : int;
    mutable refs      : int;
    mutable set_status: int;
    mutable set_pr    : int;
  }

  let zero () = {
    events = 0; prs = 0; status = 0; set_status = 0; set_pr = 0; refs = 0;
  }

  let pp ppf t =
    Fmt.pf ppf "event:%d prs:%d status:%d refs:%d set-status:%d set-pr:%d"
      t.events t.prs t.status t.refs t.set_status t.set_pr

  let equal x y = Pervasives.compare x y = 0
end

module API = struct
  type repo = {
    user  : string;
    repo  : string;
    mutable status: (string * Status.t list) list;
    mutable prs   : PR.t list;
    mutable events: Event.t list;
    mutable refs  : Ref.t list;
  }

  type user = {
    mutable repos : repo String.Map.t;
  }

  type state = {
    mutable users : user String.Map.t;
    mutable webhooks: (DK.Branch.t -> (unit, DK.error) Result.result Lwt.t) list;
    ctx: Counter.t;
  }

  let apply_webhooks t priv =
    let rec aux = function
      | []   -> ok ()
      | h::t -> h priv >>*= fun () -> aux t
    in
    aux t.webhooks >>*= fun () ->
    t.webhooks <- [];
    ok ()

  type token = state

  let lookup t ~user ~repo =
    match String.Map.find user t.users with
    | None -> None
    | Some user -> String.Map.find repo user.repos

  let lookup_exn t ~user ~repo =
    match lookup t ~user ~repo with
    | None -> failwith (Fmt.strf "Unknown user/repo: %s/%s" user repo)
    | Some repo -> repo

  let user_exists t ~user = Lwt.return (String.Map.mem user t.users)
  let repo_exists t ~user ~repo = Lwt.return (lookup t ~user ~repo <> None)
  let repos t ~user =
    match String.Map.find user t.users with
    | None -> Lwt.return_nil
    | Some user -> String.Map.dom user.repos |> String.Set.elements |> Lwt.return

  let status t ~user ~repo ~commit =
    t.ctx.Counter.status <- t.ctx.Counter.status + 1;
    let repo = lookup_exn t ~user ~repo in
    try Lwt.return (List.assoc commit repo.status)
    with Not_found -> Lwt.return_nil

  let set_status_aux t s =
    let { Status.repo; user; _ } = s in
    let repo = lookup_exn t ~user ~repo in
    let commit = s.Status.commit in
    let keep (c, _) = c <> commit in
    let status = List.filter keep repo.status in
    let rest =
      try
        List.find (fun x -> not (keep x)) repo.status
        |> snd
        |> List.filter (fun y -> y.Status.context <> s.Status.context)
      with Not_found ->
        []
    in
    let status = (commit, s :: rest) :: status in
    repo.status <- status;
    let w br =
      DK.Branch.with_transaction br (fun tr ->
          Conv.update_status tr s >>*= fun () ->
          DK.Transaction.commit tr ~message:"webhook"
        )
    in
    t.webhooks <- w :: t.webhooks

  let set_status t s =
    t.ctx.Counter.set_status <- t.ctx.Counter.set_status + 1;
    set_status_aux t s;
    Lwt.return_unit

  let set_pr_aux t pr =
    let { PR.user; repo; _ } = pr in
    let repo = lookup_exn t ~user ~repo in
    let num = pr.PR.number in
    let prs = List.filter (fun pr -> pr.PR.number <> num) repo.prs in
    repo.prs <- pr :: prs

  let set_pr t pr =
    t.ctx.Counter.set_pr <- t.ctx.Counter.set_pr + 1;
    set_pr_aux t pr;
    Lwt.return_unit

  let set_ref_aux t r =
    let { Ref.user; repo; _ } = r in
    let repo = lookup_exn t ~user ~repo in
    let name = r.Ref.name in
    let refs = List.filter (fun r -> r.Ref.name <> name) repo.refs in
    repo.refs <- r :: refs

  let apply_events t =
    t.users |> String.Map.iter @@ fun _ user ->
    user.repos |> String.Map.iter @@ fun _ repo ->
    repo.events |> List.iter @@ function
    | Event.PR pr    -> set_pr_aux t pr
    | Event.Status s -> set_status_aux t s
    | Event.Ref r    -> set_ref_aux t r
    | Event.Other _  -> ()

  let prs t ~user ~repo =
    t.ctx.Counter.prs <- t.ctx.Counter.prs + 1;
    let repo = lookup_exn t ~user ~repo in
    Lwt.return repo.prs

  let events t ~user ~repo =
    t.ctx.Counter.events <- t.ctx.Counter.events + 1;
    let repo = lookup_exn t ~user ~repo in
    Lwt.return repo.events

  let refs t ~user ~repo =
    t.ctx.Counter.refs <- t.ctx.Counter.refs + 1;
    let repo = lookup_exn t ~user ~repo in
    Lwt.return repo.refs

end

module VG = Sync(API)(DK)

let user = "test"
let repo = "test"
let pub = "test-pub"
let priv = "test-priv"

let s1 = {
  Status.context = ["foo"; "bar"; "baz"];
  url            = None;
  description    = Some "foo";
  state          = `Pending;
  commit         = "bar";
  user; repo;
}

let s2 = {
  Status.context = ["foo"; "bar"; "toto"];
  url            = Some "toto";
  description    = None;
  state          = `Failure;
  commit         = "bar";
  user; repo;
}

let s3 = {
  Status.context = ["foo"; "bar"; "baz"];
  url            = Some "titi";
  description    = Some "foo";
  state          = `Success;
  commit         = "foo";
  user; repo;
}

let s4 = {
  Status.context = ["foo"];
  url            = None;
  description    = None;
  state          = `Pending;
  commit         = "bar";
  user; repo;
}

let s5 = {
  Status.context = ["foo"; "bar"; "baz"];
  url            = Some "titi";
  description    = None;
  state          = `Failure;
  commit         = "foo";
  user; repo;
}

let pr1 =
  { PR.number = 1; state = `Open  ; head = "foo"; title = "";  user; repo }

let pr2 =
  { PR.number = 1; state = `Closed; head = "foo"; title = "foo"; user; repo }

let pr3 =
  { PR.number = 2; state = `Open  ; head = "bar"; title = "bar"; user; repo }

let pr4 =
  { PR.number = 2; state = `Open  ; head = "bar"; title = "toto"; user; repo }

let ref1 ={ Ref.user; repo; name = ["heads";"master"]; head = "bar" }
let ref2 ={ Ref.user; repo; name = ["heads";"master"]; head = "foo" }

let events0 = [
  Event.PR pr1;
  Event.PR pr2;
  Event.PR pr3;
  Event.Status s1;
  Event.Status s2;
  Event.Status s3;
  Event.Status s4;
]

let events1 = [
  Event.PR pr1;
  Event.PR pr4;
  Event.Status s1;
  Event.Status s2;
  Event.Status s3;
  Event.Status s4;
]

let prs0    = [pr1; pr3]
let status0 = [s1; s2; s3; s4]
let refs0   = [ref1]

let status_state: Status_state.t Alcotest.testable =
  (module struct include Status_state let equal = (=) end)

let snapshot: Snapshot.t Alcotest.testable =
  (module struct include Snapshot let equal x y = Snapshot.compare x y = 0 end)

let diff: Diff.t Alcotest.testable =
  (module struct include Diff let equal = (=) end)

let diffs = Alcotest.slist diff Diff.compare

let d id = { Diff.user; repo; id }

let counter: Counter.t Alcotest.testable = (module Counter)

let test_snapshot () =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  Test_utils.run (fun _repo conn ->
      let dk = DK.connect conn in
      DK.branch dk "test-snapshot" >>*= fun br ->
      let update ~prs ~status ~refs =
        let err e = Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e in
        DK.Branch.with_transaction br (fun tr ->
            Lwt_list.iter_p (fun pr ->
                Conv.update_pr tr pr >>= function
                | Ok ()   -> Lwt.return_unit
                | Error e -> err e
              ) prs
            >>= fun () ->
            Lwt_list.iter_p (fun s ->
                Conv.update_status tr s >>= function
                | Ok ()   -> Lwt.return_unit
                | Error e -> err e
              ) status
            >>= fun () ->
            Lwt_list.iter_p (fun r ->
                Conv.update_ref tr r >>= function
                | Ok ()   -> Lwt.return_unit
                | Error e -> err e
              ) refs
            >>= fun () ->
            Conv.snapshot Conv.(tree_of_transaction tr) >>*= fun s ->
          DK.Transaction.commit tr ~message:"init" >>*= fun () ->
          ok s)
      in
      update ~prs:prs0 ~status:status0 ~refs:refs0 >>*= fun s ->
      expect_head br >>*= fun head ->
      let se =
        let prs = PR.Set.of_list prs0 in
        let status = Status.Set.of_list status0 in
        let refs = Ref.Set.of_list refs0 in
        Snapshot.create ~prs ~status ~refs ()
      in
      Conv.snapshot Conv.(tree_of_commit head) >>*= fun sh ->
      Alcotest.(check snapshot) "snap transaction" se s;
      Alcotest.(check snapshot) "snap head" se sh;

      update ~prs:[pr2] ~status:[] ~refs:[] >>*= fun s1 ->
      expect_head br >>*= fun head1 ->
      let tree1 = Conv.tree_of_commit head1 in
      Conv.diff tree1 head >>*= fun diff1 ->
      Alcotest.(check diffs) "diff1" [d (`PR 1)] (Diff.Set.elements diff1);
      Conv.snapshot ~old:(head, s) tree1 >>*= fun sd ->
      Alcotest.(check snapshot) "snap diff" s1 sd;

      update ~prs:[] ~status:[s5] ~refs:[ref2] >>*= fun s2 ->
      expect_head br >>*= fun head2 ->
      let tree2 = Conv.tree_of_commit head2 in
      Conv.diff tree2 head1 >>*= fun diff2 ->
      Alcotest.(check diffs) "diff2"
        [d (`Status ("foo", ["foo";"bar";"baz"]));
         d (`Ref ["heads";"master"])]
        (Diff.Set.elements diff2);
      Conv.snapshot ~old:(head , s ) tree2 >>*= fun sd1 ->
      Conv.snapshot ~old:(head1, s1) tree2 >>*= fun sd2 ->
      Alcotest.(check snapshot) "snap diff1" s2 sd1;
      Alcotest.(check snapshot) "snap diff2" s2 sd2;

      DK.Branch.with_transaction br (fun tr ->
          DK.Transaction.make_dirs tr (p "test/toto") >>*= fun () ->
          DK.Transaction.create_or_replace_file tr ~dir:(p "test/toto") "FOO"
            (v "") >>*= fun () ->
          DK.Transaction.commit tr ~message:"test/foo"
        ) >>*= fun () ->
      expect_head br >>*= fun head3 ->
      let tree3 = Conv.tree_of_commit head3 in
      Conv.diff tree3 head2 >>*= fun diff3 ->
      let d = { Diff.user; repo = "toto"; id = `Unknown } in
      Alcotest.(check diffs) "diff3" [d] (Diff.Set.elements diff3);

      Lwt.return_unit
    )

let init status refs events =
  let tbl = Hashtbl.create (List.length status) in
  List.iter (fun s ->
      let v =
        try Hashtbl.find tbl s.Status.commit
        with Not_found -> []
      in
      Hashtbl.replace tbl s.Status.commit (s :: v)
    ) status;
  let status = Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [] in
  let ctx = Counter.zero () in
  let users = String.Map.singleton user {
      API.repos = String.Map.singleton repo {
          API.user; repo; status; refs; prs = []; events
        }
    } in
  { API.users; ctx; webhooks = [] }

let run f () =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  Test_utils.run (fun _repo conn ->
      let dk = DK.connect conn in
      DK.branch dk pub  >>*= fun pub ->
      DK.branch dk priv >>*= fun priv ->
      let t = init [] [] [] in
      let s = VG.empty in
      VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun _s ->
      DK.Branch.with_transaction pub (fun tr ->
          let dir = Datakit_path.(empty / user / repo) in
          DK.Transaction.make_dirs tr dir >>*= fun () ->
          DK.Transaction.create_or_replace_file tr ~dir
            "README" (Cstruct.of_string "trigger an import of test/test\n")
          >>*= fun () ->
          DK.Transaction.commit tr ~message:"init"
        )
      >>*= fun () ->
      f dk
    )

let check_dirs = Alcotest.(check (slist string String.compare))
let check_data msg x y = Alcotest.(check string) msg x (Cstruct.to_string y)

let check name tree =
  (* check test/test/commit *)
  let commit = Datakit_path.empty / user / repo / "commit" in
  DK.Tree.exists_dir tree commit >>*= fun exists ->
  Alcotest.(check bool) (name ^ " commit dir exists")  exists true;
  DK.Tree.read_dir tree commit >>*= fun dirs ->
  check_dirs "commits" ["bar"] dirs;
  DK.Tree.read_dir tree (commit / "bar"/ "status" ) >>*= fun dirs ->
  check_dirs "status 0" ["foo"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" ) >>*= fun dirs ->
  check_dirs "status 1" ["state";"bar"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar")
  >>*= fun dirs ->
  check_dirs "status 2" ["baz";"toto"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar" / "baz")
  >>*= fun dirs ->
  check_dirs "status 3" ["description";"state"] dirs;
  DK.Tree.read_file tree
    (commit / "bar" / "status" / "foo" / "bar" / "baz" / "state")
  >>*= fun data ->
  check_data "status/state" "pending\n" data;
  DK.Tree.read_file tree
    (commit / "bar" / "status" / "foo" / "bar" / "baz" / "description")
  >>*= fun data ->
  check_data "status/description" "foo\n" data;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar" / "toto")
  >>*= fun dirs ->
  check_dirs "status 3" ["target_url";"state"] dirs;

  (* check test/test/pr *)
  let pr = Datakit_path.empty / user / repo / "pr" in
  DK.Tree.exists_dir tree pr >>*= fun exists ->
  Alcotest.(check bool) "pr dir exists" true exists;
  DK.Tree.read_dir tree pr >>*= fun dirs ->
  check_dirs "pr 1" ["2"] dirs ;
  DK.Tree.read_dir tree (pr / "2") >>*= fun dirs ->
  check_dirs "pr 2" dirs ["state"; "head"; "title"];
  DK.Tree.read_file tree (pr / "2" / "state") >>*= fun data ->
  check_data "state" "open\n" data;
  DK.Tree.read_file tree (pr / "2" / "head") >>*= fun data ->
  check_data "head" "bar\n" data ;

  Lwt.return_unit

open Counter

let test_events dk =
  let t = init status0 refs0 events0 in
  API.apply_events t;
  let s = VG.empty in
  DK.branch dk priv >>*= fun priv ->
  DK.branch dk pub  >>*= fun pub  ->
  Alcotest.(check counter) "counter: 0"
    { events = 0; prs = 0; status = 0; refs = 0; set_pr = 0; set_status = 0 }
    t.API.ctx;
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 1; status = 1; refs = 1; set_pr = 0; set_status = 0 }
    t.API.ctx;
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun _s ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 1; refs = 1; set_pr = 0; set_status = 0 }
    t.API.ctx;
  expect_head priv >>*= fun head ->
  check "priv" (DK.Commit.tree head) >>= fun () ->
  expect_head pub >>*= fun head ->
  check "pub" (DK.Commit.tree head)

let update_status br dir state =
  DK.Branch.with_transaction br (fun tr ->
      let dir = dir / "status" / "foo" / "bar" / "baz" in
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let state = Cstruct.of_string  (Status_state.to_string state ^ "\n") in
      DK.Transaction.create_or_replace_file tr ~dir "state" state >>*= fun () ->
      DK.Transaction.commit tr ~message:"Test"
    )

let find_status t ~user ~repo =
  let repo = API.lookup_exn t ~user ~repo in
  try List.find (fun (c, _) -> c = "foo") repo.API.status |> snd |> List.hd
  with Not_found -> Alcotest.fail "foo not found"

let test_updates dk =
  let t = init status0 refs0 events1 in
  API.apply_events t;
  let s = VG.empty in
  DK.branch dk priv >>*= fun priv ->
  DK.branch dk pub  >>*= fun pub ->
  Alcotest.(check counter) "counter: 0"
    { events = 0; prs = 0; status = 0; refs = 0; set_pr = 0; set_status = 0 }
    t.API.ctx;
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 1; status = 2; refs = 1; set_pr = 0; set_status = 0 }
    t.API.ctx;
  VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 1'"
    { events = 0; prs = 1; status = 2; refs = 1; set_pr = 0; set_status = 0 }
    t.API.ctx;

  (* test status update *)
  let dir = Datakit_path.empty / user / repo / "commit" / "foo" in
  expect_head priv >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist private commit/foo" true exists;
  expect_head priv >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist private commit/foo" true exists;
  update_status pub dir `Pending >>*= fun () ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 2; refs = 1; set_pr = 0; set_status = 1 }
    t.API.ctx;
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 3"
    { events = 0; prs = 1; status = 2; refs = 1; set_pr = 0; set_status = 1 }
    t.API.ctx;
  let status = find_status t ~user ~repo in
  Alcotest.(check status_state) "update status" `Pending status.Status.state;

  (* test PR update *)
  let dir = Datakit_path.empty / user / repo / "pr" / "2" in
  expect_head priv >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist private commit/foo" true exists;
  expect_head priv >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist commit/foo" true exists;
  DK.Branch.with_transaction pub (fun tr ->
      DK.Transaction.create_or_replace_file tr ~dir
        "title" (Cstruct.of_string "hahaha\n")
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"Test"
    ) >>*= fun () ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun _s ->
  Alcotest.(check counter) "counter: 4"
    { events = 0; prs = 1; status = 2; refs = 1; set_pr = 1; set_status = 1 }
    t.API.ctx;
  let pr =
    let repo = API.lookup_exn t ~user ~repo in
    try List.find (fun pr -> pr.PR.number = 2) repo.API.prs
    with Not_found -> Alcotest.fail "foo not found"
  in
  Alcotest.(check string) "update pr's title" "hahaha" pr.PR.title;
  Lwt.return_unit

let test_startup dk =
  let t = init status0 refs0 events1 in
  API.apply_events t;
  let s = VG.empty in
  DK.branch dk priv >>*= fun priv ->
  DK.branch dk pub  >>*= fun pub ->
  let dir = Datakit_path.empty / user / repo / "commit" / "foo" in

  (* start from scratch *)
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 0; status = 0; refs = 0; set_pr = 0; set_status = 0 }
    t.API.ctx;
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 2; refs = 1; set_pr = 0; set_status = 0 }
    t.API.ctx;
  update_status pub dir `Pending >>*= fun () ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 3"
    { events = 0; prs = 1; status = 2; refs = 1; set_pr = 0; set_status = 1 }
    t.API.ctx;

  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun _s ->
  Alcotest.(check counter) "counter: 3'"
    { events = 0; prs = 1; status = 2; refs = 1; set_pr = 0; set_status = 1 }
    t.API.ctx;

  (* stop the app, apply the webhooks and restart *)
  let s = VG.empty in
  API.apply_webhooks t priv >>*= fun () ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 4"
    { events = 0; prs = 2; status = 4; refs = 2; set_pr = 0; set_status = 1 }
    t.API.ctx;
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun _s ->
  Alcotest.(check counter) "counter: 4'"
    { events = 0; prs = 2; status = 4; refs = 2; set_pr = 0; set_status = 1 }
    t.API.ctx;

  (* restart with dirty public branch *)
  let s = VG.empty in
  update_status pub dir `Failure >>*= fun () ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun s ->
  Alcotest.(check counter) "counter: 5"
    { events = 0; prs = 3; status = 6; refs = 3; set_pr = 0; set_status = 2 }
    t.API.ctx;
  let status = find_status t ~user ~repo in
  Alcotest.(check status_state) "update status" `Failure status.Status.state;

  (* receive new webhooks *)
  t.API.webhooks <- [fun br -> update_status br dir `Failure];
  API.apply_webhooks t priv >>*= fun () ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun _s ->
  Alcotest.(check counter) "counter: 6"
    { events = 0; prs = 3; status = 6; refs = 3; set_pr = 0; set_status = 2 }
    t.API.ctx;

  (* changes done in the public branch are never overwritten
     FIXME: we might want to improve/change this in the future. *)
  t.API.webhooks <- [fun br -> update_status br dir `Success];
  API.apply_webhooks t priv >>*= fun () ->
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun _s ->
  Alcotest.(check counter) "counter: 7"
    { events = 0; prs = 3; status = 6; refs = 3; set_pr = 0; set_status = 2 }
    t.API.ctx;
  let status_dir = dir / "status" / "foo" / "bar" / "baz" in
  expect_head pub >>*= fun h ->
  let tree = DK.Commit.tree h in
  DK.Tree.exists_dir tree status_dir >>*= fun dir_exists ->
  Alcotest.(check bool) "dir exists" true dir_exists;
  DK.Tree.exists_file tree (status_dir / "state") >>*= fun file_exists ->
  Alcotest.(check bool) "file exists" true file_exists;
  DK.Tree.read_file tree (status_dir / "state") >>*= fun buf ->
  Alcotest.(check string) "webhook update" "failure\n" (Cstruct.to_string buf);

  Lwt.return_unit

module Users = struct
  type t = API.user String.Map.t
  let pp_status f = function
    | `Open   -> Fmt.string f "open"
    | `Closed -> Fmt.string f "closed"

  let pp_pr f pr = Fmt.pf f "n=%d;head=%s;title=%S;%a" pr.PR.number pr.PR.head pr.PR.title pp_status pr.PR.state
  let pp_state f (commit, states) =
    Fmt.pf f "%s->%a" commit (Fmt.Dump.list Status.pp) states
  let pp_repo f { API.status; prs; _ } =
    Fmt.pf f "prs=%a;@,commits=%a"
      (Fmt.Dump.list pp_pr) prs
      (Fmt.Dump.list pp_state) status
  let pp_user f { API.repos } = String.Map.dump pp_repo f repos
  let pp f users = String.Map.dump pp_user f users

  let status_equal a b =
    a.Status.state       = b.Status.state &&
    a.Status.description = b.Status.description &&
    a.Status.url         = b.Status.url

  let equal_commit a b =
    let to_map x = List.map (fun a -> String.concat ~sep:"/" a.Status.context, a) x |> String.Map.of_list in
    let a = to_map a in
    let b = to_map b in
    String.Map.equal status_equal a b
  let equal_state a b =
    let to_map x =
      x
      |> List.filter (fun (_, status) -> status <> [])
      |> String.Map.of_list
    in
    let a = to_map a in
    let b = to_map b in
    String.Map.equal equal_commit a b
  let equal_pr a b =
    a.PR.title = b.PR.title &&
    a.PR.head = b.PR.head
  let equal_prs a b =
    let to_map x = x |> List.map (fun a -> String.of_int a.PR.number, a) |> String.Map.of_list in
    let a = to_map a in
    let b = to_map b in
    String.Map.equal equal_pr a b
  let equal_repo a b =
    equal_state a.API.status b.API.status &&
    equal_prs a.API.prs b.API.prs
  let equal_user a b =
    String.Map.equal equal_repo a.API.repos b.API.repos
  let equal a b =
    String.Map.equal equal_user a b
end
let users = (module Users : Alcotest.TESTABLE with type t = Users.t)

let opt_read_file tree path =
  DK.Tree.read_file tree path >|= function
  | Ok data -> Some (String.trim (Cstruct.to_string data))
  | Error (`Msg "No such file or directory") -> None
  | Error (`Msg x) -> failwith x

let rec read_state ~user ~repo ~commit tree path context =
  DK.Tree.read_dir tree path >>= function
  | Error _ -> Lwt.return []
  | Ok items ->
  let open Datakit_path.Infix in
  DK.Tree.read_file tree (path / "state") >>= begin function
  | Error _ -> Lwt.return []
  | Ok status ->
    opt_read_file tree (path / "description") >>= fun description ->
    opt_read_file tree (path / "target_url") >>= fun url ->
    let state =
      let status = String.trim (Cstruct.to_string status) in
      match Status_state.of_string status with
      | None -> failwith (Fmt.strf "Bad state %S" status)
      | Some x -> x in
    Lwt.return [{ Status.user; repo; commit; state; context; description; url }]
  end
  >>= fun this_state ->
  items |> Lwt_list.map_s (function
      | "status" | "description" | "target_url" -> Lwt.return []
      | item -> read_state ~user ~repo ~commit tree (path / item) (context @ [item])
    )
  >>= fun children ->
  Lwt.return (this_state @ List.concat children)

let read_opt_dir tree path =
  DK.Tree.read_dir tree path >|= function
  | Ok items -> items
  | Error (`Msg "No such file or directory") -> []
  | Error (`Msg x) -> failwith x

let read_commits tree ~user ~repo =
  let path = Datakit_path.of_steps_exn [user; repo; "commit"] in
  read_opt_dir tree path >>=
  Lwt_list.map_s (fun commit ->
      let path = Datakit_path.of_steps_exn [user; repo; "commit"; commit; "status"] in
      read_state ~user ~repo ~commit tree path [] >>= fun states ->
      Lwt.return (commit, states)
    )

let read_prs tree ~user ~repo =
  let path = Datakit_path.of_steps_exn [user; repo; "pr"] in
  read_opt_dir tree path >>=
  Lwt_list.map_s (fun number ->
      let path = Datakit_path.of_steps_exn [user; repo; "pr"; number] in
      let number = int_of_string number in
      let read name =
        DK.Tree.read_file tree (path / name) >>*= fun data ->
        Lwt.return (String.trim (Cstruct.to_string data))
      in
      read "head" >>= fun head ->
      read "title" >>= fun title ->
      Lwt.return { PR.user; repo; number; state = `Open; title; head }
    )

let state_of_branch b =
  expect_head b >>*= fun head ->
  let tree = DK.Commit.tree head in
  DK.Tree.read_dir tree Datakit_path.empty >>*=
  Lwt_list.fold_left_s (fun acc user ->
      let path = Datakit_path.of_steps_exn [user] in
      DK.Tree.read_dir tree path >>*=
      Lwt_list.fold_left_s (fun acc repo ->
          read_commits tree ~user ~repo >>= fun status ->
          read_prs tree ~user ~repo >>= fun prs ->
          let v = { API.user; repo; status; prs; refs = []; events = [] } in    (* TODO: refs *)
          String.Map.add repo v acc
          |> Lwt.return
        ) String.Map.empty
      >>= fun repos ->
      Lwt.return (String.Map.add user { API.repos } acc)
    ) String.Map.empty

let ensure_in_sync ~msg github pub =
  state_of_branch pub >>= fun pub_users ->
  Fmt.pr "GitHub:@\n@[%a@]@.\
          DataKit:@\n@[%a@]@."
         Users.pp github.API.users
         Users.pp pub_users;
  assert (Users.equal github.API.users pub_users);
  Alcotest.check users msg github.API.users pub_users;
  Lwt.return ()

let test_contexts = [|
  ["ci"; "datakit"; "test"];
  ["ci"; "datakit"; "build"];
  ["ci"; "circleci"];
|]

let random_choice ~random options =
  options.(Random.State.int random (Array.length options))

let random_state ~random ~user ~repo ~old_prs ~old_commits =
(*   let n_prs = Random.State.int random 4 in *)
  let n_prs = if (user, repo) = ("a", "a") then Random.State.int random 3 else 0 in
  let old_prs = List.rev old_prs in
  let old_commits = String.Map.of_list old_commits in
  let next_pr = ref (
      match old_prs with
      | [] -> 0
      | x::_ -> x.PR.number + 1
    )
  in
  let old_prs = old_prs |> List.map (fun pr ->
      let state =
        match pr.PR.state with
(*         | `Open when Random.State.bool random -> `Closed *)  (* TODO: Depends on #235 *)
        | s -> s
      in
      let head = random_choice ~random [| "123"; "456"; "789" |] in
      { pr with PR.state; head }
    )
  in
  let commits = ref (String.Map.dom old_commits) in
  let rec make_prs acc = function
    | 0 -> acc
    | n ->
      let commit = random_choice ~random [| "123"; "456"; "789" |] in
      commits := String.Set.add commit !commits;
      let number = !next_pr in
      incr next_pr;
      let pr = {
        PR.user;
        repo;
        number;
        state = `Open;
        head = commit;
        title = "PR#" ^ string_of_int number
      } in
      make_prs (pr :: acc) (n - 1)
  in
  let prs = make_prs old_prs n_prs |> List.rev in
  let commits =
    String.Set.elements !commits |> List.map (fun commit ->
        let description = Some "Testing..." in
        let old_status = String.Map.find commit old_commits |> default [] in
        let status =
          test_contexts |> Array.to_list |> List.map (fun context ->
              if Random.State.bool random then (
                match List.find (fun s -> s.Status.context = context) old_status with
                | exception Not_found -> []
                | old_status -> [old_status]    (* GitHub can't delete statuses *)
              ) else (
                let state = random_choice ~random [| `Pending; `Success; `Failure; `Error |] in
                [{ Status.user; repo; state; commit; description; url = None; context }]
              )
            )
          |> List.concat
        in
        commit, status
      )
  in
  prs, commits

let random_repos ?(old=String.Map.empty) ~random =
  let user_names = ["a"; "b"] in
  let repo_names = ["a"; "b"] in
  user_names |> List.map (fun user ->
      let old_user = String.Map.find user old |> default { API.repos = String.Map.empty } in
      let repos =
        repo_names |> List.map (fun repo ->
            let old_prs, old_commits =
              match String.Map.find repo old_user.API.repos with
              | None -> [], []
              | Some repo -> repo.API.prs, repo.API.status
            in
            let prs, status = random_state ~random ~user ~repo ~old_prs ~old_commits in
            repo, { API.user; repo; status; prs; refs = []; events = [] }       (* TODO: refs *)
          )
        |> String.Map.of_list
      in
      user, { API.repos }
    )
  |> String.Map.of_list

let test_random _repo conn =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  let random = Random.State.make [| 1; 2; 3 |] in
  let dk = DK.connect conn in
  DK.branch dk pub  >>*= fun pub ->
  DK.Branch.with_transaction pub (fun t ->
      let monitor ~user ~repo =
        let dir = Datakit_path.of_steps_exn [user; repo] in
        DK.Transaction.make_dirs t dir >>*= fun () ->
        DK.Transaction.create_file t ~dir "README" (Cstruct.of_string "Monitor")
      in
      monitor ~user:"a" ~repo:"a" >>*= fun () ->
      monitor ~user:"a" ~repo:"b" >>*= fun () ->
      monitor ~user:"b" ~repo:"a" >>*= fun () ->
      monitor ~user:"b" ~repo:"b" >>*= fun () ->
      DK.Transaction.commit t ~message:"Monitor repos"
    )
  >>*= fun () ->
  DK.branch dk priv >>*= fun priv ->
  let s = VG.empty in
  let users = random_repos ~random ?old:None in
  let ctx = Counter.zero () in
  let t = { API.users; ctx; webhooks = [] } in
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun _s ->
  ensure_in_sync ~msg:"init" t pub >>= fun () ->
  let users = random_repos ~random ~old:users in
  let t = { API.users; ctx; webhooks = [] } in
  VG.sync ~policy:`Once s ~pub ~priv ~token:t >>= fun _s ->
  ensure_in_sync ~msg:"update" t pub


let test_set = [
  "snapshot", `Quick, test_snapshot;
  "events"  , `Quick, run test_events;
  "updates" , `Quick, run test_updates;
  "startup" , `Quick, run test_startup;
  "random"  , `Quick, fun () -> Test_utils.run test_random;
]
