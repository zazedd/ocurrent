open Prometheus

let namespace = "ocurrent"
let subsystem = "nix"

let nix_build_events =
  let help = "Nix build events" in
  Gauge.v ~help ~namespace ~subsystem "build_events"
