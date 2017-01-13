open! Astring
open Lwt.Infix
open CI_utils.Infix

module DK = CI_utils.DK

module Image = struct
  type t = {
    id : string;
  }

  let v id = { id }

  let id t = t.id

  let pp f t = Fmt.string f t.id
end

(* Check that [path] is a valid path in [base] and contains no symlink components,
   or "..". *)
let validate_path ~base path =
  let rec aux base = function
    | [] -> ()
    | ".." :: _ -> CI_utils.failf "'..' in Dockerfile path %S!" path
    | x :: xs ->
      let subpath = Filename.concat base x in
      match Unix.lstat subpath with
      | {Unix.st_kind = Unix.S_REG | Unix.S_DIR; _} -> aux subpath xs
      | _ -> CI_utils.failf "Not a regular file or directory: %S" path
      | exception Unix.Unix_error(Unix.ENOENT, _, _) -> CI_utils.failf "File %S does not exist" subpath
      | exception ex -> CI_utils.failf "Bad file %S: %a" subpath CI_utils.pp_exn ex
  in
  aux base (String.cuts ~sep:"/" path)

module Builder = struct

  module Key = struct
    type t = {
      src : CI_git.commit;
      from : Image.t option;
    }
  end

  type t = {
    label : string;
    dockerfile : string;
    timeout : float;
  }

  type context = CI_s.job_id

  type value = Image.t

  let name t =
    Fmt.strf "docker build -f %S" t.dockerfile

  let title t _key = name t

  let load _t tree _key =
    DK.Tree.read_file tree CI_cache.Path.value >>*= fun data ->
    Lwt.return (Image.v (String.trim (Cstruct.to_string data)))

  let branch_safe_char = function
    | ':' -> '-'
    | x -> x

  let branch t {Key.src; from} =
    let from =
      match from with
      | None -> ""
      | Some from -> "-from-" ^ String.map branch_safe_char (Image.id from)
    in
    Printf.sprintf "docker-build-%s-of-%s%s" t.label (CI_git.hash src) from

  let rewrite_from ?from path =
    match from with
    | None -> Lwt.return ()
    | Some base ->
      Lwt_io.with_file ~mode:Lwt_io.input path (fun ch -> Lwt_io.read ch) >>= fun contents ->
      match String.cut ~sep:"\n" contents with
      | None -> CI_utils.failf "Missing newline in %S" path
      | Some (first, rest) ->
        if not (String.is_prefix ~affix:"FROM " (String.Ascii.uppercase first)) then
          CI_utils.failf "Dockerfile %S starts %S, not 'FROM '" path first;
        let contents = Fmt.strf "FROM %s@\n%s" (Image.id base) rest in
        Lwt_io.with_file ~mode:Lwt_io.output path (fun ch -> Lwt_io.write ch contents)

  let generate t ~switch ~log trans job_id key =
    let {Key.src; from} = key in
    let output = CI_live_log.write log in
    CI_git.with_clone ~log ~job_id src (fun srcdir ->
        CI_utils.with_timeout ~switch t.timeout @@ fun switch ->
        validate_path ~base:srcdir t.dockerfile;
        let dockerpath = Filename.concat srcdir t.dockerfile in
        rewrite_from ?from dockerpath >>= fun () ->
        let cwd = Filename.dirname dockerpath in
        let cmd = [| "docker"; "build"; "-f"; t.dockerfile; "." |] in
        CI_process.run ~cwd ~switch ~output ("", cmd) >>= fun () ->
        let cmd = [| "docker"; "build"; "-q"; "-f"; t.dockerfile; "." |] in
        let buffer = Buffer.create 64 in
        let output = Buffer.add_string buffer in
        CI_process.run ~cwd ~switch ~output ("", cmd) >>= fun () ->
        let image = Image.v (String.trim (Buffer.contents buffer)) in
        let data = Cstruct.of_string (Image.id image) in
        DK.Transaction.create_file trans CI_cache.Path.value data >>*= fun () ->
        Lwt.return (Ok image)
      )
end

module Build_cache = CI_cache.Make(Builder)

type t = Build_cache.t

let create ~logs ~timeout ~label dockerfile =
  Build_cache.create ~logs { Builder.label; dockerfile; timeout }

let build t ?from src =
  let open! CI_term.Infix in
  CI_term.job_id >>= fun job_id ->
  Build_cache.find t job_id {Builder.Key.src; from}
