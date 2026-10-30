(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Standard footer for all pages *)

open Htmlit

(* Local change (webpage fork): the footer used to hard-code Anil's links.
   It is now replaceable via [set]; the default shows only the feed links. *)

let feeds =
  El.p [
    El.em [
      El.txt "Feed (";
      El.a ~at:[At.href "/news.xml"] [
        El.txt "Atom ";
        El.img ~at:[At.class' "inline-icon"; At.alt "atom"; At.src "/assets/rss.svg"] ()
      ];
      El.txt " / ";
      El.a ~at:[At.href "/feed.json"] [
        El.txt "JSON ";
        El.img ~at:[At.class' "inline-icon"; At.alt "json"; At.src "/assets/rss.svg"] ()
      ];
      El.txt ")"
    ]
  ]

let current = ref feeds

let set html = current := html

let footer () = !current
