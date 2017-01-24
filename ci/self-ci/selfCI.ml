open Datakit_ci

let minute = 60.

let pool = Monitored_pool.create "Docker" 10

module Dockerfile = struct
  (* [v ~timeout file] is a caching builder for [file]. *)
  let v ?label ~timeout file =
    let label = label |> Utils.default file in
    Docker.create ~logs ~pool ~timeout ~label file

  let prometheus  = v ~timeout:(30. *. minute) "Dockerfile.prometheus"
  let client      = v ~timeout:(30. *. minute) "Dockerfile.client"
  let ci          = v ~timeout:(30. *. minute) "Dockerfile.ci"
  let self_ci     = v ~timeout:(30. *. minute) "ci/self-ci/Dockerfile" ~label:"Dockerfile.self-ci"
  let server      = v ~timeout:(30. *. minute) "Dockerfile.server"
  let github      = v ~timeout:(30. *. minute) "Dockerfile.github"
  let local_git   = v ~timeout:(30. *. minute) "Dockerfile.bridge-local-git"
  let datakit     = v ~timeout:(30. *. minute) "Dockerfile"
end

let alpine_4_02 = Term.return (Docker.Image.of_published "ocaml/opam:alpine_ocaml-4.02.3")

let opam_test pkg =
  let cmd = Fmt.strf "opam remove %S && opam install -t --deps-only %S && opam reinstall -v -t %S" pkg pkg pkg in
  Docker.command ~logs ~pool ~timeout:(30. *. minute) ~label:("test-" ^ pkg) ["sh"; "-c"; cmd]

let opam_test_ci = opam_test "datakit-ci"

module Tests = struct
  open Term.Infix

  (* Rules to create the images from the Dockerfiles.
     Note that we may build multiple images from the same Dockerfile (to test different bases).
     We also define the dependencies between the builds here. *)
  let images src =
    let build ?from dockerfile =
      match from with
      | None -> src >>= Docker.build dockerfile ?from:None
      | Some from ->
        Term.without_logs (Term.pair src from) >>= fun (src, from) ->
        Docker.build dockerfile ~from src
    in
    object(self)
      method client      = build Dockerfile.client
      method client_4_02 = build Dockerfile.client     ~from:alpine_4_02
      method local_git   = build Dockerfile.local_git  ~from:self#client
      method prometheus  = build Dockerfile.prometheus
      method ci          = build Dockerfile.ci
      method self_ci     = build Dockerfile.self_ci    ~from:self#ci
      method server      = build Dockerfile.server
      method github      = build Dockerfile.github     ~from:self#server
      method datakit     = build Dockerfile.datakit    ~from:self#server
    end

  let check_builds term =
    term >|= fun (_:Docker.Image.t) -> "Build succeeded"

  let run_tests image tests =
    image >>= Docker.run tests >|= fun () -> "Tests passed"

  (* [datakit repo target] is the set of tests to run on [target]. *)
  let datakit repo = function
    | `Ref (_, ["heads"; "gh-pages"]) -> []       (* Don't try to build the gh-pages branch *)
    | target ->
      let src = Git.fetch_head repo target in
      let images = images src in
      [
        "server",      check_builds images#server;
        "prometheus",  check_builds images#prometheus;
        "client",      check_builds images#client;
        "client-4.02", check_builds images#client_4_02;
        "ci",          run_tests    images#ci           opam_test_ci;
        "self-ci",     check_builds images#self_ci;
        "github",      check_builds images#github;
        "datakit",     check_builds images#datakit;
        "local-git",   check_builds images#local_git;
      ]
end

let projects repo = [
  Config.project ~id:"docker/datakit" (Tests.datakit repo)
]

let make_config ?state_repo ~listen_addr ~remote () =
  let repo = Git.v ~logs ~remote "/data/repos/datakit" in
  let web_config =
    Web.config
      ~name:"datakit-ci"
      ?state_repo
      ~can_read:ACL.everyone
      ~can_build:ACL.(username "admin")
      ~listen_addr
      ()
  in
  Config.v ~web_config ~projects:(projects repo)

let config_for = function
  | `Production ->
    make_config
      ~remote:"https://github.com/docker/datakit.git"
      ~state_repo:(Uri.of_string "https://github.com/docker/datakit.logs")
      ~listen_addr:(`HTTP 8080) (* We live behind an nginx proxy. *)
      ()
  | `Localhost ->
    (* We pull from a shared volume, not from GitHub, and we don't push the results. *)
    make_config
      ~remote:"/mnt/datakit"
      ~listen_addr:(`HTTP 8080) (* Don't bother with TLS when testing locally. *)
      ()

(* Command-line parsing *)

open Cmdliner

let profiles = [
  "production", `Production;    (* Running on Docker Cloud *)
  "localhost",  `Localhost;     (* Running locally with docker-compose *)
]

(* The "--profile=PROFILE" option *)
let profile =
  let doc =
    Arg.info ["profile"]
      ~docv:"PROFILE"
      ~doc:"Which configuration profile to use."
  in
  Arg.(value @@ opt (enum profiles) `Production doc)

let () =
  Datakit_ci.run Cmdliner.Term.(pure config_for $ profile)
