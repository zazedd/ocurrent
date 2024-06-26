open Current.Syntax

(* User defines a flake, or a derivation
   We provide the way for the user to build, spawn a shell,
   container, etc. with that flake
*)

module Nix = struct
  module CC = Current_cache.Make (Nix_cmd)

  let build_command command ~args ?level ?schedule ?timeout ?flake ?path ?pool
      commit =
    let flake =
      match flake with
      | None -> `Path (Fpath.v "", "")
      | Some (`Path _ as f) -> f
      | Some (`Contents c) -> `Contents c
    in
    let lock =
      match Sys.file_exists "flake.lock" with
      | true -> Some (Fpath.v "./flake.lock")
      | false -> None
    in
    CC.get ?schedule { pool; timeout; level }
      { Nix_cmd.Key.commit; flake; lock; command; args; path }
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
    Current.component "shell%a" pp_sp_label label
    |>
    let> commit = get_build_context src and> flake = Current.option_seq flake in
    Nix.build_command `Develop ~args ?level ?schedule ?timeout ?flake ?path
      ?pool commit
end
