open Lwt.Infix

type t = {
  pool : unit Current.Pool.t option;
  timeout : Duration.t option;
  level : Current.Level.t option;
}

type flake = {
  (* `Path (/path/to/somewhere/with/a/flake.nix, name) -> nix build /path/to/somewhere/with/a#name *)
  content : [ `Path of Fpath.t | `Contents of string ];
  name : string;
}

let id = "nix-command"

module Key = struct
  type t = {
    commit : [ `No_context | `Git of Current_git.Commit.t | `Dir of Fpath.t ];
    flake : flake;
    lock : Fpath.t option;
    command : [ `Build | `Run | `Develop ];
    args : string list;
    path : Fpath.t option;
  }

  let digest_flake = function
    | { content = `Path path; name } ->
        `Assoc [ ("flake: path", `String (Fpath.to_string path ^ "#" ^ name)) ]
    | { content = `Contents contents; name } ->
        `Assoc
          [
            ( "flake: contents",
              `String
                ((Digest.string contents |> Digest.to_hex) ^ ", name: " ^ name)
            );
          ]

  let source_to_json = function
    | `No_context -> `Null
    | `Git commit -> `String (Current_git.Commit.hash commit)
    | `Dir path -> `String (Fpath.to_string path)

  let pp_args = Fmt.(list ~sep:sp (quote string))

  let to_json { commit; flake; lock; command; args; path } =
    let lock =
      match lock with
      | Some p -> "lock file at: " ^ Fpath.to_string p
      | None -> "no lock file"
    in
    `Assoc
      [
        ("commit", source_to_json commit);
        ("flake", digest_flake flake);
        ("lock", `String lock);
        ("command", `String (Cmd.nix_command_to_string command));
        ("args", [%derive.to_yojson: string list] args);
        ( "path",
          Option.(
            value ~default:`Null
              (map (fun v -> `String (Fpath.to_string v)) path)) );
      ]

  let digest t = t |> to_json |> Yojson.Safe.to_string
  let pp f t = Yojson.Safe.pretty_print f (to_json t)
end

module Value = Current.Unit

let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

let handle_context ~job context fn =
  let open Lwt_result.Infix in
  match context with
  | `No_context -> Current.Process.with_tmpdir ~prefix:"build-context-" fn
  | `Dir path ->
      Current.Process.with_tmpdir ~prefix:"build-context-" @@ fun dir ->
      Current.Process.exec ~cwd:dir ~cancellable:false ~job
        ("", [| "rsync"; "-aHq"; Fpath.to_string path ^ "/"; "." |])
      >>= fun () -> fn dir
  | `Git commit -> Current_git.with_checkout ~job commit fn

let build { pool; timeout; level } job key =
  let { Key.commit; flake; lock; command; args; path } = key in
  (match flake with
  | { content = `Contents contents; _ } ->
      Current.Job.log job "@[<v2>Using Flake:@,%a@]" Fmt.lines contents
  | { content = `Path _; _ } -> ());
  let level = Option.value level ~default:Current.Level.Average in
  Current.Job.start ?timeout job ?pool ~level >>= fun () ->
  handle_context ~job commit @@ fun dir ->
  let dir = match path with Some path -> Fpath.(dir // path) | None -> dir in
  let flake_file =
    match flake with
    | { content = `Contents contents; name } ->
        Bos.OS.File.write Fpath.(dir / "flake.nix") (contents ^ "\n")
        |> or_raise;
        [ ".#" ^ name ]
    | { content = `Path path; name } ->
        if Fpath.to_string path = "." then
          [ Fpath.(to_string dir) ^ "#" ^ name ]
        else [ Fpath.(to_string (dir // path)) ^ "#" ^ name ]
  in
  let _ =
    match lock with
    | Some x ->
        Log.info (fun f ->
            f "flake.lock file provided: %s" (x |> Fpath.to_string))
    | None ->
        Log.warn (fun f ->
            f
              "No flake.lock file provided, this outcome will NOT be \
               reproducible! Please, provide a lock file.")
  in
  let cmd = Cmd.nix command (flake_file @ args) in
  (Current.Process.exec ~cancellable:false ~job cmd >|= function
   | Error _ as e -> e
   | Ok () ->
       Bos.OS.File.read Fpath.(dir / "flake.lock")
       |> Stdlib.Result.map @@ fun lock_content ->
          Log.info (fun f ->
              f "Built Nix Derivation. Lock file: %s" lock_content);
          ())
  >|= fun res ->
  Prometheus.Gauge.dec_one Metrics.nix_build_events;
  res

let pp = Key.pp
let auto_cancel = true

(*
TODO: search for the flake.lock file on the git repository as well instead of only localy
*)
