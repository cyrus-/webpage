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

    Just the feed links, under the page content; the social links are in
    the sidebar (layout/sidebar.html). arod's feeds carry only notes, so
    that is what the footer calls them. *)

let set_footer () =
  let open Htmlit in
  let feed href label =
    El.a ~at:[ At.href href ]
      [ El.txt (label ^ " "); El.img ~at:[ At.class' "inline-icon"; At.alt ""; At.src "/assets/rss.svg" ] () ]
  in
  Arod.Footer.set
    (El.p [ El.em [ El.txt "Notes feed ("; feed "/news.xml" "Atom"; El.txt " / "; feed "/feed.json" "JSON"; El.txt ")" ] ])

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
let flora_default = "/flora/bloom-forest.svg"

(* Set while rendering a page that should be framed for reading: a note on
   its own page, as shared by its link. *)
let reading = ref false

let as_reading_page render =
  reading := true;
  Fun.protect ~finally:(fun () -> reading := false) render

let set_frame ~live () =
  let open Htmlit in
  let header = read_file "layout/header.html" in
  let sidebar = read_file "layout/sidebar.html" in
  let intro = read_file "layout/intro.html" in
  let tab_bar =
    El.nav ~at:[ At.class' "tab-bar" ]
      (List.map (fun (label, href) -> El.a ~at:[ At.class' "tab"; At.href href ] [ El.txt label ]) tabs)
  in
  (* botanical border, shown only on wide windows (see Flora) *)
  let flora =
    El.aside
      ~at:[ At.class' "flora"; At.v "aria-hidden" "true" ]
      [ El.img ~at:[ At.src flora_default; At.v "alt" ""; At.v "loading" "lazy" ] () ]
  in
  Arod.Page.set_frame (fun ~header:_ ~content ~footer ->
      [
        El.unsafe_raw header;
        (if !reading then
           (* just the note, in a column of its own: no sidebar, intro or tabs *)
           El.div ~at:[ At.class' "reading-area" ]
             [
               El.div ~at:[ At.class' "reading" ]
                 [ El.a ~at:[ At.class' "back"; At.href "/notes" ] [ El.txt "← All notes" ]; content; footer ];
               flora;
             ]
         else
           El.div ~at:[ At.class' "main-area" ]
             [
               El.div ~at:[ At.class' "left-col" ] [ El.unsafe_raw sidebar ];
               El.div ~at:[ At.class' "right-col" ]
                 [ El.unsafe_raw intro; tab_bar; El.div ~at:[ At.class' "content-grid" ] [ content ]; footer ];
               flora;
             ]);
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

(* The recorded talk for a paper, if any. When there are several (say a
   conference talk and a longer one elsewhere), the one from the paper's own
   venue, which its title names. *)
let paper_video p =
  let slug = Bushel.Paper.slug p in
  let at_venue v =
    let t = Bushel.Video.title v and s = venue p in
    let n = String.length s in
    let rec go i = i + n <= String.length t && (String.sub t i n = s || go (i + 1)) in
    go 0
  in
  match List.filter (fun v -> Bushel.Video.paper v = Some slug) (Arod.Model.videos ()) with
  | [] -> None
  | vs -> Some (Option.value (List.find_opt at_venue vs) ~default:(List.hd vs))

(* The links under a paper's venue are small line icons, named in a tooltip. *)
let link_icons =
  [
    ("PDF", {|<path d="M4 1.5h5.5L13 5v9.5H4z"/><path d="M9.5 1.5V5H13M6.2 8.5h4.6M6.2 11h4.6"/>|});
    ( "DOI",
      {|<path d="M6.8 9.2a2.8 2.8 0 0 0 4 0l2.3-2.3a2.8 2.8 0 0 0-4-4l-1 1"/><path d="M9.2 6.8a2.8 2.8 0 0 0-4 0L2.9 9.1a2.8 2.8 0 0 0 4 4l1-1"/>|}
    );
    ( "BIB",
      {|<path d="M6 2c-1.6 0-2 .6-2 2v2.2c0 1-.6 1.6-1.6 1.8 1 .2 1.6.8 1.6 1.8V12c0 1.4.4 2 2 2M10 2c1.6 0 2 .6 2 2v2.2c0 1 .6 1.6 1.6 1.8-1 .2-1.6.8-1.6 1.8V12c0 1.4-.4 2-2 2"/>|}
    );
    ("VIDEO", {|<rect x="1.5" y="3" width="13" height="10" rx="2.5"/><path d="M6.6 5.7v4.6L10.4 8z" fill="currentColor"/>|});
    ( "URL",
      {|<circle cx="8" cy="8" r="6.3"/><path d="M1.7 8h12.6M8 1.7c1.8 1.8 2.7 3.9 2.7 6.3S9.8 12.5 8 14.3C6.2 12.5 5.3 10.4 5.3 8S6.2 3.5 8 1.7"/>|}
    );
  ]

let icon_link name href =
  let open Htmlit in
  El.a
    ~at:[ At.href href; At.title name; At.v "aria-label" name ]
    [
      El.unsafe_raw
        (Printf.sprintf
           {|<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>|}
           (List.assoc name link_icons));
    ]

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
        (if pdf then Some (icon_link "PDF" ("/papers/" ^ slug ^ ".pdf")) else None);
        Option.map (fun d -> icon_link "DOI" ("https://doi.org/" ^ d)) (Bushel.Paper.doi p);
        Some (icon_link "BIB" ("/papers/" ^ slug ^ ".bib"));
        Option.map (fun u -> icon_link "URL" u) (Bushel.Paper.url p);
        Option.map (fun v -> icon_link "VIDEO" ("https://www.youtube.com/watch?v=" ^ Bushel.Video.uuid v)) (paper_video p);
      ]
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
(* A note on its own page gets the reading frame. (The notes feeds live
   under /notes/ too.) *)
let note_page cfg slug req =
  match slug with
  | "atom.xml" -> Arod.Handlers.atom_feed cfg req
  | "feed.json" -> Arod.Handlers.json_feed cfg req
  | _ -> as_reading_page (fun () -> Arod.Handlers.note ((), slug) req)

let routes cfg =
  let arod = Route.Routes.fold (fun r acc -> r :: acc) (Arod.Handlers.all_routes cfg) [] in
  Route.Routes.of_list
    ([
       Route.get_ [] (home cfg);
       Route.get_ [ "papers" ] (papers_tab cfg);
       Route.get_ [ "talks" ] (talks_tab cfg);
       Route.get_ [ "videos" ] (talks_tab cfg);
       Route.get Route.(exact "notes" @/ param) (fun ((), slug) req -> note_page cfg slug req);
       Route.get Route.(exact "notes" @/ param @/ exact "") (fun ((), (slug, ())) req -> note_page cfg slug req);
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

(* Draft notes show in the preview server but are left out of the built
   site: its pages, listings and feeds. *)
let drop_drafts () =
  let e = Arod.Model.get_entries () in
  let module E = Bushel.Entry in
  Arod.Model.entries :=
    Some
      (E.v
         ~papers:(E.papers e @ E.old_papers e)
         ~notes:(List.filter (fun n -> not (Bushel.Note.draft n)) (E.notes e))
         ~projects:(E.projects e) ~ideas:(E.ideas e) ~videos:(E.videos e) ~contacts:(E.contacts e)
         ~images:(E.images e) ~doi_entries:(E.doi_entries e) ~data_dir:(E.data_dir e) ())

let build cfg ~out ~prefix ~extra =
  use_repo_contacts cfg;
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let _entries = Arod.Model.init ~cfg fs in
  drop_drafts ();
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
