module Server = Cohttp_lwt_unix.Server
module Path = Cohttp.Path

let string_of_timestamp time =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } = time in
  Fmt.str "%04d-%02d-%02d %02d:%02d:%02d" (tm_year + 1900) (tm_mon + 1) tm_mday tm_hour tm_min tm_sec

let ps_href ?(root = false) ~prefix h =
  let r = if root then "/" else "" in
  match prefix with
  | None -> h
  | Some p -> r ^ p ^ h

let p_href ?(root = false) ~prefix h =
  let open Tyxml.Html in
  ps_href ~root ~prefix h |> a_href

let p_action ?(root = false) ~prefix h =
  let open Tyxml.Html in
  ps_href ~root ~prefix h |> a_action

let p_data ?(root = false) ~prefix h =
  let open Tyxml.Html in
  ps_href ~root ~prefix h |> a_data
