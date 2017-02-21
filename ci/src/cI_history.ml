open CI_utils.Infix
open Lwt.Infix
open Astring

module DK = CI_utils.DK

let metadata_commit_path = Datakit_path.of_string_exn "metadata-commit"

let index_branch = "commit-index"

let ( / ) = Datakit_path.Infix.( / )

module State = struct
  type t = {
    parents : string list;
    jobs : string CI_output.t String.Map.t;
  }

  let empty = { parents = []; jobs = String.Map.empty }

  let jobs c = c.jobs

  let parents c = c.parents

  let equal {parents; jobs} b =
    parents = b.parents &&
    String.Map.equal CI_output.equal jobs b.jobs

  let pp =
    Fmt.braces (fun f {parents; jobs} ->
        Fmt.pf f "parents = %a;@ jobs = %a"
          (Fmt.Dump.list Fmt.string) parents
          (String.Map.dump (CI_output.pp Fmt.string)) jobs
      )
end

type target = {
  branch_name : string;
  lock : Lwt_mutex.t;
  mutable commit : State.t option;
}

module Saved_output = struct
  let to_cstruct t = Cstruct.of_string (Yojson.Basic.to_string t)

  let of_cstruct c =
    let json = Yojson.Basic.from_string (Cstruct.to_string c) in
    CI_output.of_json json
end

type t = {
  cache : (string, target) Hashtbl.t;   (* Branch name -> target *)
}

let create () =
  let cache = Hashtbl.create 100 in
  { cache }

let load commit =
  let tree = DK.Commit.tree commit in
  DK.Commit.parents commit >>*= fun parents ->
  let parents = List.map DK.Commit.id parents in
  begin DK.Tree.read_dir tree (Datakit_path.of_steps_exn ["job"]) >>= function
    | Ok items ->
      items |> Lwt_list.filter_map_p (fun job_name ->
          let path = Datakit_path.of_steps_exn ["job"; job_name; "output"] in
          DK.Tree.read_file tree path >|= function
          | Ok data -> Some (job_name, Saved_output.of_cstruct data)
          | Error `Does_not_exist -> None
          | Error x -> failwith (Fmt.to_to_string DK.pp_error x)
        )
    | Error `Does_not_exist -> Lwt.return []
    | Error x -> failwith (Fmt.to_to_string DK.pp_error x)
  end
  >>= fun jobs ->
  let jobs = String.Map.of_list jobs in
  Lwt.return { State.parents; jobs }

let lookup t dk target =
  let branch_name = CI_target.status_branch target in
  match Hashtbl.find t.cache branch_name with
  | t -> Lwt_mutex.with_lock t.lock (fun () -> Lwt.return t)    (* Ensures we've finished loading *)
  | exception Not_found ->
    let target = { lock = Lwt_mutex.create (); commit = None; branch_name } in
    Hashtbl.add t.cache branch_name target;
    (* No-one can interrupt us here, so we will certainly get the lock first. *)
    Lwt_mutex.with_lock target.lock (fun () ->
        DK.branch dk branch_name >>*= fun branch ->
        DK.Branch.head branch >>*= function
        | None -> Lwt.return ()
        | Some head ->
          load head >|= fun commit ->
          target.commit <- Some commit
      )
    >>= fun () ->
    Lwt.return target

let diff _id prev next =
  match prev, next with
  | Some prev, Some next when CI_output.equal prev next -> None
  | _, Some next -> Some (`Write next)
  | Some _, None -> Some `Delete
  | None, None -> assert false

let index_dir ~repo ~hash =
  let {Datakit_github.Repo.user; repo} = repo in
  Datakit_path.of_steps_exn [user; repo; "commit"; hash]

let record t dk ~src_hash input jobs =
  Lwt_mutex.with_lock t.lock @@ fun () ->
  let state = t.commit |> CI_utils.default State.empty in
  let patch = String.Map.merge diff state.State.jobs jobs in
  if String.Map.is_empty patch then (
    (* Update state to include live logs and non-archived saved logs. *)
    t.commit <- Some {state with State.jobs};
    Lwt.return ()
  ) else (
    let open! Datakit_path.Infix in
    let messages = ref [] in
    let add_msg fmt =
      fmt |> Fmt.kstrf @@ fun msg ->
      CI_utils.Log.info (fun f -> f "Record: %s: %s" t.branch_name msg);
      messages := msg :: !messages
    in
    DK.branch dk t.branch_name >>*= fun branch ->
    let metadata_commit = Cstruct.of_string (DK.Commit.id input) in
    DK.Branch.with_transaction branch (fun tr ->
        DK.Transaction.create_or_replace_file tr metadata_commit_path metadata_commit >>*= fun () ->
        String.Map.bindings patch |> Lwt_list.iter_s (function
            | (job, `Delete) ->
              let dir = Datakit_path.of_steps_exn ["job"; job] in
              add_msg "Remove old job %s" job;
              DK.Transaction.remove tr dir >>*= Lwt.return
            | (job, `Write output) ->
              let dir = Datakit_path.of_steps_exn ["job"; job] in
              add_msg "%s -> %s" job (CI_output.descr output);
              let json = CI_output.json_of output in
              let data = Saved_output.to_cstruct json in
              DK.Transaction.make_dirs tr dir >>*= fun () ->
              DK.Transaction.create_or_replace_file tr (dir / "output") data >>*= Lwt.return
          )
        >>= fun () ->
        DK.Transaction.parents tr >>*= fun parents ->
        begin match !messages with
          | [] -> assert false
          | [message] ->
            DK.Transaction.commit tr ~message
          | m :: _ as ms ->
            let message =
              Fmt.strf "%d updates (%s, ...)@.@.%a"
                (List.length ms)
                m
                Fmt.(vbox (list ~sep:(const cut ()) string)) ms
            in
            DK.Transaction.commit tr ~message
        end >>*= fun () ->
        let parents = List.map DK.Commit.id parents in
        Lwt.return (Ok parents)
      )
    >>*= fun parents ->
    let state = { State.jobs; parents } in
    t.commit <- Some state;
    (* todo: Transaction.commit should return the new commit object directly *)
    DK.Branch.head branch >>*= function
    | None -> assert false
    | Some state_commit ->
      let state_commit = DK.Commit.id state_commit in
      let target = CI_target.of_status_branch t.branch_name in
      let repo = CI_target.repo target in
      let dir = index_dir ~repo ~hash:src_hash in
      let data = Cstruct.of_string state_commit in
      let sub_name = Fmt.to_to_string CI_target.Branch_escape.pp_sub target in
      let message = Fmt.strf "Update %s:%s -> %s" sub_name src_hash state_commit in
      DK.branch dk index_branch >>*= fun index ->
      DK.Branch.with_transaction index (fun tr ->
          DK.Transaction.make_dirs tr dir >>*= fun () ->
          DK.Transaction.create_file tr (dir / sub_name) data >>*= fun () ->
          DK.Transaction.commit tr ~message
        )
      >>*= Lwt.return
  )

let head t = t.commit

let builds_of_commit dk c =
  DK.branch dk index_branch >>*= fun branch ->
  DK.Branch.head branch >>*= function
  | None -> Lwt.return CI_target.Map.empty
  | Some head ->
    let tree = DK.Commit.tree head in
    let open Datakit_github in
    let {Commit.repo; hash} = c in
    let dir = index_dir ~repo ~hash in
    DK.Tree.read_dir tree dir >>= function
    | Error `Does_not_exist -> Lwt.return CI_target.Map.empty
    | Error x -> failwith (Fmt.to_to_string DK.pp_error x)
    | Ok branches ->
      branches |> Lwt_list.map_p (fun b ->
          DK.Tree.read_file tree (dir / b) >>*= fun data ->
          let state_commit = DK.commit dk (Cstruct.to_string data) in
          match CI_target.Branch_escape.parse_sub ~repo b with
          | Some target -> Lwt.return (target, state_commit)
          | None -> CI_utils.failf "Invalid branch name in index: %S" b
        )
      >|= CI_target.Map.of_list

