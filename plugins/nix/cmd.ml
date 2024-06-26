(*let experimental_features = [ "--extra-experimental-features nix-command"; "--extra-experimental-features flakes" ]*)

let nix_command_to_string = function
  | `Build -> "build" 
  | `Run -> "run" 
  | `Develop -> "develop" 

let nix command args =
  let command = nix_command_to_string command in
  "", Array.of_list ("nix" :: [ command; "--verbose" ] @ args)

let nix_shell args =
  "", Array.of_list ("nix-shell" :: args)

let nix_build args =
  "", Array.of_list ("nix-build" :: args)

let pp f (prog, args) =
  if prog <> "" then Fmt.pf f "[%S] " prog;
  Fmt.(list ~sep:sp (quote string)) f (Array.to_list args)
