open Current.Syntax

(* User defines a flake, or a derivation
   We provide the way for the user to build, spawn a shell,
   container, etc. with that flake
*)

module Nix_cmd = Nix_cmd

module Nix = struct
  module CC = Current_cache.Output (Nix_cmd)

  let build_command command ~args ?level ?schedule ?timeout ?flake ?path ?pool
      commit =
    let flake =
      let open Nix_cmd in
      match flake with
      | None -> { content = `Path (Fpath.v "."); name = "" }
      | Some f -> f
    in
    let lock_location =
      match path with
      | Some p -> Fpath.(p / "flake.lock")
      | None -> Fpath.v "flake.lock"
    in
    let lock =
      match Bos.OS.File.read lock_location with
      | Ok _ -> Some lock_location
      | Error _ -> None
    in
    CC.set ?schedule { pool; timeout; level }
      { Nix_cmd.Key.commit; lock; command; args; path } { Nix_cmd.Value.flake }
end

module Default = struct
  let pp_sp_label = Fmt.(option (sp ++ string))

  let get_build_context = function
    | `No_context -> Current.return `No_context
    | `Git commit -> Current.map (fun x -> `Git x) commit
    | `Dir path -> Current.map (fun path -> `Dir path) path

  let build ?level ?schedule ?timeout ?label ?flake ?path ?pool ~args src =
    Current.component "build%a" pp_sp_label label
    |>
    let> commit = get_build_context src and> flake = Current.option_seq flake in
    Nix.build_command `Build ~args ?level ?schedule ?timeout ?flake ?path ?pool
      commit

  let shell ?level ?schedule ?timeout ?label ?flake ?path ?pool ~args src =
    (*
      [
        [ "Rscript"; "--version" ];
        [ "echo"; "hello" ];
      ]
      ->
      [ "-c"; "bash"; "-c" "Rscript --version && echo hello" ];
    *)
    let args =
      List.mapi
        (fun i arg_list ->
          if i <> List.length args - 1 then arg_list @ [ "&&" ] else arg_list)
        args
      |> List.fold_left
           (fun acc arg_list ->
             acc ^ List.fold_left (fun acc2 arg -> acc2 ^ " " ^ arg) "" arg_list)
           ""
    in
    let args = [ "-c"; "bash"; "-c"; args ] in
    Current.component "shell%a" pp_sp_label label
    |>
    let> commit = get_build_context src and> flake = Current.option_seq flake in
    Nix.build_command `Develop ~args ?level ?schedule ?timeout ?flake ?path
      ?pool commit
end
