open Lwt.Infix

type t = {
  pool : unit Current.Pool.t option;
  timeout : Duration.t option;
  level : Current.Level.t option;
}

let id = "nix-command"

module Key = struct
  type t = {
    commit : [ `No_context | `Git of Current_git.Commit.t | `Dir of Fpath.t ];
    (* `File (/path/to/somewhere/with/flake/, name) -> nix build /path/to/somewhere/with/flake/#name *)
    flake : [ `Path of Fpath.t * string | `Contents of string ];
    lock : Fpath.t option;
    command : [ `Build | `Run | `Develop ];
    args : string list;
    path : Fpath.t option;
  }

  let digest_flake = function
    | `Path (path, name) ->
        `Assoc [ ("path", `String (Fpath.to_string path ^ "#" ^ name)) ]
    | `Contents contents ->
        `Assoc
          [ ("contents", `String (Digest.string contents |> Digest.to_hex)) ]

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
      Current.Process.exec ~cwd:dir ~cancellable:true ~job
        ("", [| "rsync"; "-aHq"; Fpath.to_string path ^ "/"; "." |])
      >>= fun () -> fn dir
  | `Git commit -> Current_git.with_checkout ~job commit fn

let build { pool; timeout; level } job key =
  let { Key.commit; flake; lock; command; args; path } = key in
  (match flake with
  | `Contents contents ->
      Current.Job.log job "@[<v2>Using Flake:@,%a@]" Fmt.lines contents
  | `Path _ -> ());
  let level = Option.value level ~default:Current.Level.Average in
  Current.Job.start ?timeout job ?pool ~level >>= fun () ->
  handle_context ~job commit @@ fun dir ->
  let dir = match path with Some path -> Fpath.(dir // path) | None -> dir in
  let flake_file =
    match flake with
    | `Contents contents ->
        (* need a way to define name *)
        Bos.OS.File.write Fpath.(dir / "flake.nix") (contents ^ "\n")
        |> or_raise;
        [ ".#" ]
    | `Path (path, name) -> [ Fpath.(to_string (dir // path)) ^ "#" ^ name ]
  in
  let _ =
    match lock with
    | Some x ->
        Log.info (fun f ->
            f "flake.lock file provided: %s" (x |> Fpath.to_string))
    | None ->
        Log.warn (fun f ->
            f
              "No flake.lock file provided, this outcome would not be \
               reproducible! Current_nix will create a lock file for you.")
  in
  let cmd = Cmd.nix command (flake_file @ args) in
  (Current.Process.exec ~cancellable:true ~job cmd >|= function
   | Error _ as e -> e
   | Ok () ->
       Bos.OS.File.read Fpath.(dir / "flake.lock")
       |> Stdlib.Result.map @@ fun content ->
          Log.info (fun f -> f "Built Nix Derivation. Lock file: %s" content);
          Log.info (fun f ->
              f
                "Writing lock file to local directory. TODO: git add and \
                 commit this");
          (* write to the location of the flake.nix *)
          Bos.OS.File.write (Fpath.v "flake.lock") content |> ignore;
          ())
  >|= fun res ->
  Prometheus.Gauge.dec_one Metrics.nix_build_events;
  res

let pp = Key.pp
let auto_cancel = true

(*

TODO: search for the flake.lock file on the git repository as well instead of only localy
TODO: add, commit and possibly push the flake.lock file to the git repo, if there was no flake.lock in the first place

DONE
-> provide a way to chain commands for Nix.shell, possibly a list of lists 

*)
