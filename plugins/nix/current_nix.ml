open Current.Syntax

(* User defines a flake, or a derivation 
We provide the way for the user to build, spawn a shell,
container, etc. with that flake
*)

module Nix = struct
  module BC = Current_cache.Make(Build)
  let build ~args ?level ?schedule ?timeout ?flake ?path ?pool commit =
    let flake =
      match flake with
      | None -> `File (Fpath.v "flake.nix", "")
      | Some (`File _ as f) -> f
      | Some (`Contents c) -> `Contents c
    in
    BC.get ?schedule { pool; timeout; level }
    { Build.Key.commit; flake; args; path }


end

module Default = struct
  let pp_sp_label = Fmt.(option (sp ++ string))

  let get_build_context = function
    | `No_context -> Current.return `No_context
    | `Git commit -> Current.map (fun x -> `Git x) commit
    | `Dir path -> Current.map (fun path -> `Dir path) path

  let build ?level ?schedule ?timeout ?label ?flake ?path ?pool ~args src =
    Current.component "build%a" pp_sp_label label |>
    let> commit = get_build_context src
    and> flake = Current.option_seq flake in
    Nix.build ~args ?level ?schedule ?timeout ?flake ?path ?pool commit

end



