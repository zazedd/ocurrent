open Tyxml.Html

let render_result = function
  | Ok () -> [txt "Success!"]
  | Error (`Active `Waiting_for_confirmation) -> [txt "Waiting for confirmation..."]
  | Error (`Active `Ready) -> [txt "Ready..."]
  | Error (`Active `Running) -> [txt "Running..."]
  | Error (`Msg msg) -> [txt ("ERROR: " ^ msg)]

let settings ctx config =
  let prefix = let open Context in let open Site in ctx.site.href_prefix in
  let selected = Current.Config.get_confirm config in
  let levels =
    Current.Level.values
    |> List.map @@ fun level ->
    let s = Current.Level.to_string level in
    let msg = Fmt.str "Confirm if level >= %s" s in
    let sel = if selected = Some level then [a_selected ()] else [] in
    option ~a:(a_value s :: sel) (txt msg)
  in
  let csrf = Context.csrf ctx in
  form ~a:[Utils.p_action ~prefix "/set/confirm"; a_method `Post; a_class ["settings-form"]] [
    select ~a:[a_name "level"] (
      let sel = if selected = None then [a_selected ()] else [] in
      option ~a:(a_value "none" :: sel) (txt "No confirmation required") :: List.rev levels
    );
    input ~a:[a_name "csrf"; a_input_type `Hidden; a_value csrf] ();
    input ~a:[a_input_type `Submit; a_value "Submit"] ();
  ]

let r ~engine = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    let prefix = ctx.site.href_prefix in
    let uri = Context.uri ctx in
    let config = Current.Engine.config engine in
    let { Current.Engine.value; jobs = _ } = Current.Engine.state engine in
    let verbatim_query = Uri.verbatim_query uri in
    let path = Option.value ~default:"" prefix ^ "/pipeline.svg?" ^ (Option.value verbatim_query ~default:"") in
    Context.respond_ok ctx ?refresh:ctx.site.refresh_pipeline [
      div [
        object_ ~a:[Utils.p_data ~prefix path] [txt "Pipeline diagram"];
      ];
      h2 [txt "Result"];
      p (render_result value);
      h2 [txt "Settings"];
      settings ctx config;
    ]
end
