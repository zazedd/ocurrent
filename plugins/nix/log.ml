let src = Logs.Src.create "current.nix" ~doc:"OCurrent Nix plugin"
include (val Logs.src_log src : Logs.LOG)
