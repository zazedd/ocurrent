(*let experimental_features = [ "--extra-experimental-features nix-command"; "--extra-experimental-features flakes" ]*)

let nix args =
  "", Array.of_list ("nix" :: args @ [ "--verbose" ])

let build args = nix ("build" :: args)

let run args = nix ("run" :: args)

let develop args = nix ("develop" :: args)

let shell args = nix ("shell" :: args)

let nix_shell args =
  "", Array.of_list ("nix-shell" :: args)

let nix_build args =
  "", Array.of_list ("nix-build" :: args)

let pp f (prog, args) =
  if prog <> "" then Fmt.pf f "[%S] " prog;
  Fmt.(list ~sep:sp (quote string)) f (Array.to_list args)
