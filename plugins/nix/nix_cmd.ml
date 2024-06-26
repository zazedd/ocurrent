open Lwt.Infix

type t = {
  pool : unit Current.Pool.t option;
  timeout : Duration.t option;
  level : Current.Level.t option;
}

let id = "docker-run"

module Key = struct
  type t = {
    commit : [ `No_context | `Git of Current_git.Commit.t | `Dir of Fpath.t ];
    (* `File (/path/to/somewhere, name) -> nix build /path/to/somewhere#name *)
    flake : [ `File of (Fpath.t * string) | `Contents of string ];
    command : [`Build | `Run | `Develop ];
    args : string list;
    path : Fpath.t option;
  }

  let digest_flake = function
    | `File (path, name) -> `Assoc [ "file", `String (Fpath.to_string path ^ "#" ^ name) ]
    | `Contents contents -> `Assoc [ "contents", `String (Digest.string contents |> Digest.to_hex) ]

  let source_to_json = function
    | `No_context -> `Null
    | `Git commit -> `String (Current_git.Commit.hash commit)
    | `Dir path -> `String (Fpath.to_string path)

  let pp_args = Fmt.(list ~sep:sp (quote string))

  let to_json { commit; flake; command; args; path; } =
    `Assoc [
      "commit", source_to_json commit;
      "flake", digest_flake flake;
      "command", `String (Cmd.nix_command_to_string command);
      "args", [%derive.to_yojson:string list] args;
      "path", Option.(value ~default:`Null (map (fun v -> `String (Fpath.to_string v)) path));
    ]

  let digest t = t |> to_json |> Yojson.Safe.to_string

  let pp f t = Yojson.Safe.pretty_print f (to_json t)

end

module Value = Current.Unit

let or_raise = function
  | Ok () -> ()
  | Error (`Msg m) -> raise (Failure m)

let handle_context ~job context fn =
  let open Lwt_result.Infix in
  match context with
  | `No_context -> Current.Process.with_tmpdir ~prefix:"build-context-" fn
  | `Dir path ->
      Current.Process.with_tmpdir ~prefix:"build-context-" @@ fun dir ->
      Current.Process.exec ~cwd:dir ~cancellable:true ~job ("", [| "rsync"; "-aHq"; Fpath.to_string path ^ "/"; "." |]) >>= fun () ->
      fn dir
  | `Git commit -> Current_git.with_checkout ~job commit fn

let build { pool; timeout; level } job key =
  let { Key.commit; flake; command; args; path } = key in
  begin match flake with
    | `Contents contents ->
      Current.Job.log job "@[<v2>Using Flake:@,%a@]" Fmt.lines contents
    | `File _ -> ()
  end;
  let level = Option.value level ~default:Current.Level.Average in
  Current.Job.start ?timeout job ?pool ~level >>= fun () ->
  handle_context ~job commit @@ fun dir ->
  let dir = match path with
    | Some path -> Fpath.(dir // path)
    | None -> dir
  in
  let flake_file =
    match flake with
    | `Contents contents ->
      Bos.OS.File.write Fpath.(dir / "flake.nix") (contents ^ "\n") |> or_raise;
      [ ".#" ]
    | `File (path, name) ->
      [Fpath.(to_string (dir // path)) ^ "#" ^ name]
  in
  let cmd = Cmd.nix command (flake_file @ args) in
  Current.Process.exec ~cancellable:true ~job cmd
  >|= (function
    | Error _ as e -> e
    | Ok () -> 
        Bos.OS.File.read (Fpath.v "flake.lock") |> Stdlib.Result.map @@ fun hash ->
        Log.info (fun f -> f "Built docker image %s" hash);
        ())
  >|= (fun res -> Prometheus.Gauge.dec_one Metrics.nix_build_events; res)

let pp = Key.pp

let auto_cancel = true
