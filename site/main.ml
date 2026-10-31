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

(** {1 Layout}

    The page frame follows the original hand-written site: the header with
    the tagline, a left column of cards (layout/sidebar.html) and a right
    column with the intro card (layout/intro.html), a tab bar, and then the
    page content. The partials are plain HTML with root-relative links. *)

let read_file f = In_channel.with_open_bin f In_channel.input_all

let tabs = [ ("Projects", "/"); ("Papers", "/papers"); ("Talks", "/talks"); ("Notes", "/notes") ]

(* Preview only: poll /__live and reload the page when its token changes
   (assets edited, or the server restarted by dev.sh). *)
let live_reload_script =
  {|(function () {
  var token = null;
  function check() {
    fetch("/__live", { cache: "no-store" })
      .then(function (r) { return r.text(); })
      .then(function (t) {
        if (token === null) token = t;
        else if (t !== token) location.reload();
      })
      .catch(function () {})
      .finally(function () { setTimeout(check, 700); });
  }
  check();
})();|}

(* The botanical border drawn by default; the design switcher can swap it. *)
let flora_default = "/flora/vine-forest.svg"

let set_frame ~live () =
  let open Htmlit in
  let header = read_file "layout/header.html" in
  let sidebar = read_file "layout/sidebar.html" in
  let intro = read_file "layout/intro.html" in
  let tab_bar =
    El.nav ~at:[ At.class' "tab-bar" ]
      (List.map (fun (label, href) -> El.a ~at:[ At.class' "tab"; At.href href ] [ El.txt label ]) tabs)
  in
  Arod.Page.set_frame (fun ~header:_ ~content ~footer ->
      [
        El.unsafe_raw header;
        El.div ~at:[ At.class' "main-area" ]
          [
            El.div ~at:[ At.class' "left-col" ] [ El.unsafe_raw sidebar ];
            El.div ~at:[ At.class' "right-col" ]
              [ El.unsafe_raw intro; tab_bar; El.div ~at:[ At.class' "content-grid" ] [ content ] ];
            (* botanical border, shown only on wide windows (see Flora) *)
            El.aside
              ~at:[ At.class' "flora"; At.v "aria-hidden" "true" ]
              [ El.img ~at:[ At.src flora_default; At.v "alt" ""; At.v "loading" "lazy" ] () ];
          ];
        footer;
      ]
      @ if live then [ El.script [ El.unsafe_raw live_reload_script ] ] else [])

(** {1 Home page}

    One card per project (in the order of layout/home-projects.txt), each
    listing the papers marked [selected] whose primary project (the first
    in [projects]) is that project. Other project tags only affect project
    pages and tag listings. *)

let author_name = "Cyrus Omar"

(* Paper slugs end in a venue key (e.g. 2025-grove-popl); this maps it to
   the label shown next to the paper. *)
let venues =
  [ ("popl", "POPL"); ("oopsla", "OOPSLA"); ("splashe", "SPLASH-E"); ("propl", "PROPL");
    ("vlhcc", "VL/HCC"); ("aarhus", "Aarhus"); ("hatra", "HATRA"); ("hope", "HOPE");
    ("tfp", "TFP"); ("onward", "Onward!"); ("tyde", "TyDe"); ("pldi", "PLDI"); ("icfp", "ICFP");
    ("snapl", "SNAPL"); ("gpce", "GPCE"); ("sac", "SAC"); ("ecoop", "ECOOP"); ("psp", "PSP");
    ("icse", "ICSE"); ("iwaco", "IWACO"); ("globaldsl", "GlobalDSL"); ("jneurosci", "J. Neuro.");
    ("ijhci", "IJHCI"); ("acc", "ACC"); ("icassp", "ICASSP"); ("ijcnn", "IJCNN") ]

let venue p =
  let slug = Bushel.Paper.slug p in
  let key = match String.rindex_opt slug '-' with Some i -> String.sub slug (i + 1) (String.length slug - i - 1) | None -> slug in
  let name = Option.value (List.assoc_opt key venues) ~default:(String.uppercase_ascii key) in
  Printf.sprintf "%s %d" name (Bushel.Paper.year p)

let paper_card_entry cfg p =
  let open Htmlit in
  let slug = Bushel.Paper.slug p in
  let note = Bushel.Paper.note p in
  let award = match note with Some n when n <> "Conditionally accepted" -> Some n | _ -> None in
  let authors =
    Bushel.Paper.authors p
    |> List.map (fun a -> if a = author_name then El.span ~at:[ At.class' "me-author" ] [ El.txt a ] else El.txt a)
    |> List.fold_left (fun acc a -> if acc = [] then [ a ] else acc @ [ El.txt ", "; a ]) []
  in
  let pdf = Sys.file_exists (Filename.concat cfg.Arod.Config.paths.static_dir ("papers/" ^ slug ^ ".pdf")) in
  let links =
    List.filter_map Fun.id
      [
        (if pdf then Some (El.a ~at:[ At.href ("/papers/" ^ slug ^ ".pdf") ] [ El.txt "PDF" ]) else None);
        Option.map (fun d -> El.a ~at:[ At.href ("https://doi.org/" ^ d) ] [ El.txt "DOI" ]) (Bushel.Paper.doi p);
        Some (El.a ~at:[ At.href ("/papers/" ^ slug ^ ".bib") ] [ El.txt "BIB" ]);
        Option.map (fun u -> El.a ~at:[ At.href u ] [ El.txt "URL" ]) (Bushel.Paper.url p);
      ]
    |> List.fold_left (fun acc a -> if acc = [] then [ a ] else acc @ [ El.txt " · "; a ]) []
  in
  let venue_el =
    El.div ~at:[ At.class' "paper-venue" ]
      ((El.txt (venue p)
       :: (if note = Some "Conditionally accepted" then
             [ El.br (); El.span ~at:[ At.class' "venue-note" ] [ El.txt "Conditionally" ] ]
           else []))
      @ [ El.div ~at:[ At.class' "paper-links" ] links ])
  in
  El.div ~at:[ At.class' "paper" ]
    [
      venue_el;
      El.div ~at:[ At.class' "paper-details" ]
        ([
           El.div ~at:[ At.class' "paper-title-line" ]
             [ El.a ~at:[ At.class' "major-paper"; At.href ("/papers/" ^ slug) ] [ El.txt (Bushel.Paper.title p) ] ];
           El.div ~at:[ At.class' "paper-authors" ] authors;
         ]
        @ match award with Some a -> [ El.div ~at:[ At.class' "paper-award" ] [ El.txt a ] ] | None -> []);
    ]

let project_card cfg proj =
  let open Htmlit in
  let slug = Bushel.Project.slug proj in
  let papers =
    Arod.Model.papers ()
    |> List.filter (fun p ->
           Bushel.Paper.selected p
           && match Bushel.Paper.project_slugs p with primary :: _ -> primary = slug | [] -> false)
    |> List.sort (fun a b -> compare (Bushel.Paper.year b, Bushel.Paper.slug b) (Bushel.Paper.year a, Bushel.Paper.slug a))
  in
  El.div ~at:[ At.class' "card project-card"; At.id slug ]
    ([
       El.h2 [ El.a ~at:[ At.href ("/projects/" ^ slug) ] [ El.txt (Bushel.Project.title proj) ] ];
       El.unsafe_raw (Arod.View.md_to_html (Bushel.Project.body proj));
     ]
    @ List.map (paper_card_entry cfg) papers)

(* A page whose content is a list of cards, laid out by the card-list
   styles (see the layout options in site.css). *)
let card_page cfg ~title ~kind cards =
  let open Htmlit in
  let page =
    Arod.Page.page ~title ~description:cfg.Arod.Config.site.description
      ~page_footer:(Arod.Footer.footer ())
      ~page_content:(El.div ~at:[ At.class' ("card-list " ^ kind) ] cards)
      ()
  in
  Route.Response.html (El.to_string ~doctype:true page)

let home cfg _req =
  let open Htmlit in
  let order =
    read_file "layout/home-projects.txt" |> String.split_on_char '\n' |> List.map String.trim
    |> List.filter (fun l -> l <> "" && l.[0] <> '#')
  in
  let projects = Arod.Model.projects () in
  let cards =
    List.filter_map
      (fun slug -> List.find_opt (fun p -> Bushel.Project.slug p = slug) projects |> Option.map (project_card cfg))
      order
  in
  card_page cfg ~title:cfg.Arod.Config.site.name ~kind:"project-cards" cards

(** {1 Papers and Talks tabs}

    One card per paper (newest first, same format as the home cards) and
    one card per talk (YouTube thumbnail linking to the video). *)

let papers_tab cfg _req =
  let open Htmlit in
  let cards =
    Arod.Model.papers ()
    |> List.sort (fun a b ->
           compare (Bushel.Paper.year b, Bushel.Paper.slug b) (Bushel.Paper.year a, Bushel.Paper.slug a))
    |> List.map (fun p -> El.div ~at:[ At.class' "card paper-card" ] [ paper_card_entry cfg p ])
  in
  card_page cfg ~title:"Papers" ~kind:"paper-cards" cards

let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let talks_tab cfg _req =
  let open Htmlit in
  let title_of slug f =
    match Arod.Model.lookup slug with Some (`Paper p) -> Some (f p) | _ -> None
  in
  let card v =
    let id = Bushel.Video.uuid v in
    let y, m, _ = Bushel.Video.date v in
    let watch = "https://www.youtube.com/watch?v=" ^ id in
    let paper =
      Option.bind (Bushel.Video.paper v) (fun slug ->
          title_of slug (fun p ->
              El.div ~at:[ At.class' "talk-paper" ]
                [ El.txt "Paper: "; El.a ~at:[ At.href ("/papers/" ^ slug) ] [ El.txt (Bushel.Paper.title p) ] ]))
    in
    El.div ~at:[ At.class' "card talk-card" ]
      ([
         El.a ~at:[ At.class' "talk-thumb"; At.href watch ]
           [ El.img ~at:[ At.src (Printf.sprintf "https://i.ytimg.com/vi/%s/hqdefault.jpg" id);
                          At.alt (Bushel.Video.title v); At.v "loading" "lazy" ] () ];
         El.div ~at:[ At.class' "talk-title" ] [ El.a ~at:[ At.href watch ] [ El.txt (Bushel.Video.title v) ] ];
         El.div ~at:[ At.class' "talk-date" ] [ El.txt (Printf.sprintf "%s %d" months.(m - 1) y) ];
       ]
      @ Option.to_list paper)
  in
  let cards =
    Arod.Model.videos ()
    |> List.sort (fun a b -> compare (Bushel.Video.date b) (Bushel.Video.date a))
    |> List.map card
  in
  card_page cfg ~title:"Talks" ~kind:"talk-cards" cards

(** Our routes: the home page and Papers/Talks tabs first, then arod's, then (for local preview)
    files from public/ and the CV PDF, which the static build copies in. *)
let routes cfg =
  let arod = Route.Routes.fold (fun r acc -> r :: acc) (Arod.Handlers.all_routes cfg) [] in
  Route.Routes.of_list
    ([
       Route.get_ [] (home cfg);
       Route.get_ [ "papers" ] (papers_tab cfg);
       Route.get_ [ "talks" ] (talks_tab cfg);
       Route.get_ [ "videos" ] (talks_tab cfg);
     ]
    @ List.map
        (fun (name, svg) ->
          Route.get_ [ "flora"; name ] (fun _req ->
              Route.Response.raw ~status:200
                ~headers:[ ("content-type", "image/svg+xml") ]
                (Lazy.force svg)))
        Flora.variants
    @ arod
    @ [
        Route.get_ [ "omar-cv.pdf" ] (Arod.Handlers.static_file ~dir:"." "omar-cv.pdf");
        Route.get Route.rest (fun path req ->
            let path =
              if Sys.file_exists ("public/" ^ path) && Sys.is_directory ("public/" ^ path) then
                Filename.concat path "index.html"
              else path
            in
            Arod.Handlers.static_file ~dir:"public" path req);
      ])

(** {1 Rendering} *)

type page ={ path : string; query : (string * string) list; file : string }

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
      List.map (fun (name, _) -> page ("/flora/" ^ name) ("flora/" ^ name)) Flora.variants;
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
  set_frame ~live:false ();
  let routes = routes cfg in
  let pages = pages () in
  let ok = List.filter (render routes ~prefix ~out) pages |> List.length in
  let paths = cfg.Arod.Config.paths in
  copy_tree ~src:paths.assets_dir ~dst:(Filename.concat out "assets");
  copy_tree ~src:paths.images_dir ~dst:(Filename.concat out "images");
  copy_tree ~src:paths.static_dir ~dst:(Filename.concat out "static");
  (* arod serves /papers/<slug>.pdf from <static_dir>/papers/ *)
  copy_tree ~src:(Filename.concat paths.static_dir "papers") ~dst:(Filename.concat out "papers");
  List.iter
    (fun src ->
      if Sys.is_directory src then copy_tree ~src ~dst:out
      else copy_tree ~src ~dst:(Filename.concat out (Filename.basename src)))
    extra;
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
    let doc =
      "File, or directory whose contents, to copy verbatim into the output root. Repeatable."
    in
    Arg.(value & opt_all file [] & info [ "extra" ] ~docv:"PATH" ~doc)
  in
  let run () config out prefix extra = build (load_config config) ~out ~prefix ~extra in
  Cmd.v (Cmd.info "build" ~doc:"Render the whole site to static files.")
    Term.(const run $ logging_t $ config_t $ out $ prefix $ extra)

(* The live-reload token: when this server started, plus the newest
   modification time under the assets directory (which the server reads from
   disk on every request, so asset edits need no restart). *)
let rec newest_mtime path =
  match Sys.is_directory path with
  | true ->
      Array.fold_left (fun acc f -> Float.max acc (newest_mtime (Filename.concat path f))) 0. (Sys.readdir path)
  | false -> (Unix.stat path).Unix.st_mtime
  | exception Sys_error _ -> 0.

let serve_cmd =
  let run () config =
    let cfg = load_config config in
    use_repo_contacts cfg;
    Eio_main.run @@ fun env ->
    let _entries = Arod.Model.init ~cfg (Eio.Stdenv.fs env) in
    set_footer ();
    set_frame ~live:true ();
    let memo_cache = Arod.Memo.create ~ttl:0. () in
    let started = Unix.gettimeofday () in
    let live _req =
      Route.Response.raw ~status:200
        ~headers:[ ("content-type", "text/plain"); ("cache-control", "no-store") ]
        (Printf.sprintf "%.0f-%.3f" started (newest_mtime cfg.Arod.Config.paths.assets_dir))
    in
    let routes = Route.Routes.add (Route.get_ [ "__live" ] live) (routes cfg) in
    match Arod_server.run ~config:cfg ~memo_cache routes with
    | Ok () -> ()
    | Error e -> Log.err (fun m -> m "Server error: %s" (Printexc.to_string e))
  in
  Cmd.v (Cmd.info "serve" ~doc:"Serve the site locally for preview.")
    Term.(const run $ logging_t $ config_t)

let () =
  exit (Cmd.eval (Cmd.group (Cmd.info "site" ~doc:"Build Cyrus Omar's webpage") [ build_cmd; serve_cmd ]))
