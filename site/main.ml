(* Static build and local preview for Cyrus Omar's webpage.

   Content lives in Bushel format under the configured data directory and is
   rendered with (vendored) arod. [build] calls arod's route handlers directly
   for every page and writes the results to disk, prefixing root-relative
   links so the site can live under a subdirectory such as /~comar. *)

let src = Logs.Src.create "site" ~doc:"Static site builder"

module Log = (val Logs.src_log src : Logs.LOG)
module Route = Arod.Route

(** {1 Output} *)

let rec mkdir_p dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755
  end

let write_file path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc contents)

let rec copy_tree ~src ~dst =
  if Sys.file_exists src then
    if Sys.is_directory src then
      Sys.readdir src
      |> Array.iter (fun f ->
             if f.[0] <> '.' then
               copy_tree ~src:(Filename.concat src f) ~dst:(Filename.concat dst f))
    else
      write_file dst (In_channel.with_open_bin src In_channel.input_all)

(** {1 Link prefixing}

    arod emits root-relative URLs ("/papers/..."). When the site is served
    from a subdirectory, rewrite those in HTML attributes and CSS [url(...)]
    to start with [prefix]. Protocol-relative URLs ("//host/...") are left
    alone. *)

let attr_re =
  Re.(compile (seq [
    group (seq [ alt [ str "href"; str "src"; str "action"; str "poster"; str "data-link" ]; str "=\"" ]);
    char '/'; group (alt [ eos; compl [ char '/' ] ]) ]))

let css_url_re =
  Re.(compile (seq [ group (seq [ str "url("; opt (set "\"'") ]); char '/';
                     group (alt [ eos; compl [ char '/' ] ]) ]))

let prefix_links ~prefix body =
  if prefix = "" then body
  else
    let sub re s =
      Re.replace re s ~f:(fun g -> Re.Group.get g 1 ^ prefix ^ "/" ^ Re.Group.get g 2)
    in
    body |> sub attr_re |> sub css_url_re

(** {1 Footer}

    Built from the site author's contact card (data/sortal/<handle>.yaml):
    one link per entry in [services], labelled by its [label] field. *)

let set_footer () =
  let open Htmlit in
  let author = Arod.Model.author () in
  let name = Option.fold ~none:"" ~some:Sortal_schema.Contact.name author in
  let services = Option.fold ~none:[] ~some:Sortal_schema.Contact.services author in
  let link (svc : Sortal_schema.Contact.service) =
    let label = Option.value svc.label ~default:svc.url in
    El.a ~at:[ At.rel "me"; At.class' "noicon"; At.href svc.url ] [ El.txt label ]
  in
  let rec sep = function [] -> [] | [ x ] -> [ x ] | x :: xs -> x :: El.txt " / " :: sep xs in
  let year = let (y, _, _), _ = Ptime.to_date_time (Ptime_clock.now ()) in y in
  Arod.Footer.set
    (El.splice
       [
         El.p [ El.em [ El.txt (Printf.sprintf "© %d %s. There are no third-party trackers on this site." year name) ] ];
         El.p [ El.em (sep (List.map link services)) ];
         Arod.Footer.feeds;
       ])

(** {1 Rendering} *)

type page = { path : string; query : (string * string) list; file : string }

let page ?(query = []) path file = { path; query; file }

let dir_page ?query path = page ?query path (Filename.concat path "index.html")

let render routes ~prefix ~out { path; query; file } =
  let req = Route.Request.create ~meth:`GET ~path ~query ~headers:[] in
  match Route.Routes.dispatch routes req with
  | exception e ->
      Log.warn (fun m -> m "%s: %s, skipped" path (Printexc.to_string e));
      false
  | Some resp when Route.Response.status resp = 200 ->
      let body = Route.Response.body resp in
      let body =
        if Filename.check_suffix file ".html" then prefix_links ~prefix body else body
      in
      write_file (Filename.concat out file) body;
      true
  | Some resp ->
      Log.warn (fun m -> m "%s: HTTP %d, skipped" path (Route.Response.status resp));
      false
  | None ->
      Log.warn (fun m -> m "%s: no route, skipped" path);
      false

let pages () =
  let all = [ ("all", "") ] in
  let entry_pages kind slugs = List.map (fun s -> dir_page (Printf.sprintf "/%s/%s" kind s)) slugs in
  let papers = List.map Bushel.Paper.slug (Arod.Model.papers ()) in
  List.concat
    [
      [ page "/" "index.html" ];
      List.map (dir_page ~query:all) [ "/papers"; "/notes"; "/ideas"; "/projects"; "/talks" ];
      entry_pages "papers" papers;
      List.map (fun s -> page (Printf.sprintf "/papers/%s.bib" s) (Printf.sprintf "papers/%s.bib" s)) papers;
      entry_pages "notes" (List.map Bushel.Note.slug (Arod.Model.notes ()));
      entry_pages "ideas" (List.map Bushel.Idea.slug (Arod.Model.ideas ()));
      entry_pages "projects" (List.map Bushel.Project.slug (Arod.Model.projects ()));
      entry_pages "videos" (List.map Bushel.Video.slug (Arod.Model.videos ()));
      List.map
        (fun f -> page ("/" ^ f) f)
        [ "news.xml"; "feed.json"; "perma.xml"; "perma.json"; "sitemap.xml"; "robots.txt";
          "notes/atom.xml"; "notes/feed.json" ];
    ]

(* arod looks up the site author in sortal's contact store. Keep contacts in
   the repo, as <data_dir>/sortal/<handle>.yaml, unless overridden. *)
let use_repo_contacts cfg =
  if Sys.getenv_opt "SORTAL_DATA_DIR" = None then
    let dir = cfg.Arod.Config.paths.data_dir in
    let dir = if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir in
    Unix.putenv "SORTAL_DATA_DIR" dir

let build cfg ~out ~prefix ~extra =
  use_repo_contacts cfg;
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _entries = Arod.Model.init ~cfg fs in
  set_footer ();
  let routes = Arod.Handlers.all_routes cfg in
  let pages = pages () in
  let ok = List.filter (render routes ~prefix ~out) pages |> List.length in
  let paths = cfg.Arod.Config.paths in
  copy_tree ~src:paths.assets_dir ~dst:(Filename.concat out "assets");
  copy_tree ~src:paths.images_dir ~dst:(Filename.concat out "images");
  copy_tree ~src:paths.static_dir ~dst:(Filename.concat out "static");
  (* arod serves /papers/<slug>.pdf from <static_dir>/papers/ *)
  copy_tree ~src:(Filename.concat paths.static_dir "papers") ~dst:(Filename.concat out "papers");
  List.iter (fun dir -> copy_tree ~src:dir ~dst:out) extra;
  Log.app (fun m -> m "Wrote %d of %d pages to %s" ok (List.length pages) out)

(** {1 CLI} *)

open Cmdliner

let setup_logging style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let logging_t = Term.(const setup_logging $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let config_t =
  let doc = "Site configuration file." in
  Arg.(value & opt file "site.toml" & info [ "c"; "config" ] ~docv:"FILE" ~doc)

let load_config path = Arod.Config.of_file path

let build_cmd =
  let out =
    Arg.(value & opt string "_site" & info [ "o"; "out" ] ~docv:"DIR" ~doc:"Output directory.")
  in
  let prefix =
    let doc = "Path prefix for root-relative links, e.g. /~comar. Empty for a site served at /." in
    Arg.(value & opt string "" & info [ "prefix" ] ~docv:"PATH" ~doc)
  in
  let extra =
    let doc = "Directory whose contents are copied verbatim into the output root. Repeatable." in
    Arg.(value & opt_all dir [] & info [ "extra" ] ~docv:"DIR" ~doc)
  in
  let run () config out prefix extra = build (load_config config) ~out ~prefix ~extra in
  Cmd.v (Cmd.info "build" ~doc:"Render the whole site to static files.")
    Term.(const run $ logging_t $ config_t $ out $ prefix $ extra)

let serve_cmd =
  let run () config =
    let cfg = load_config config in
    use_repo_contacts cfg;
    Eio_main.run @@ fun env ->
    let _entries = Arod.Model.init ~cfg (Eio.Stdenv.fs env) in
    set_footer ();
    let memo_cache = Arod.Memo.create ~ttl:0. () in
    match Arod_server.run ~config:cfg ~memo_cache (Arod.Handlers.all_routes cfg) with
    | Ok () -> ()
    | Error e -> Log.err (fun m -> m "Server error: %s" (Printexc.to_string e))
  in
  Cmd.v (Cmd.info "serve" ~doc:"Serve the site locally for preview.")
    Term.(const run $ logging_t $ config_t)

let () =
  exit (Cmd.eval (Cmd.group (Cmd.info "site" ~doc:"Build Cyrus Omar's webpage") [ build_cmd; serve_cmd ]))
