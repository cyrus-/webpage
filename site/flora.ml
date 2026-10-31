(* Flora: botanical borders for the right-hand margin of wide windows, after
   engraved book ornaments: a flowering vine, crowned with a Purkinje cell, a
   tree or a big flower, printed with a little risograph grain.

   Everything here is a pure function of its arguments. "Random" choices come
   from [Rng], where a value is a hash of the path of keys that leads to it,
   so every build draws the same picture without threading any state. *)

(** {1 Randomness} *)

module Rng = struct
  type t = int

  let make (seed : int) : t = Hashtbl.hash ("flora", seed)
  let key (r : t) (k : string) : t = Hashtbl.hash (r, k)
  let nth (r : t) (i : int) : t = Hashtbl.hash (r, i)
  let unit r = float_of_int (Hashtbl.hash (r, "unit") land 0xffffff) /. 16777216.
  let range r lo hi = lo +. ((hi -. lo) *. unit r)
  let chance r p = unit r < p
end

(** {1 Geometry} *)

type v = { x : float; y : float }

let v x y = { x; y }
let ( +: ) a b = v (a.x +. b.x) (a.y +. b.y)
let ( -: ) a b = v (a.x -. b.x) (a.y -. b.y)
let ( *: ) k a = v (k *. a.x) (k *. a.y)
let dir a = v (cos a) (sin a)
let angle_of a = Float.atan2 a.y a.x
let pi = Float.pi
let up = -.pi /. 2.
let deg a = a *. 180. /. pi
let lerp a b t = a +. ((b -. a) *. t)

(* How far to turn from heading [a] to face [b], in (-pi, pi]. *)
let turn_to a b = Float.rem (b -. a +. (3. *. pi)) (2. *. pi) -. pi

(* A path grown from [p] in [n] steps of length [step]; [steer p a i] gives
   the heading of step [i] from the current point and heading. *)
let grow ~step ~n p a0 steer =
  let rec go i p a acc =
    if i >= n then List.rev acc
    else
      let a = steer p a i in
      let p = p +: (step *: dir a) in
      go (i + 1) p a (p :: acc)
  in
  go 0 p a0 [ p ]

(* A path whose heading is a function of the distance travelled. *)
let curve ?(step = 2.) p length heading =
  let n = max 1 (int_of_float (length /. step)) in
  let step = length /. float n in
  grow ~step ~n p (heading 0.) (fun _ _ i -> heading (float i *. step))

(* A Catmull-Rom spline through [pts], sampled about every [step]. *)
let spline ?(step = 2.) pts =
  let a = Array.of_list pts in
  let n = Array.length a in
  let get i = a.(max 0 (min (n - 1) i)) in
  let segment i =
    let p0 = get (i - 1) and p1 = get i and p2 = get (i + 1) and p3 = get (i + 2) in
    let m = max 1 (int_of_float (Float.hypot (p2.x -. p1.x) (p2.y -. p1.y) /. step)) in
    List.init m (fun j ->
        let t = float j /. float m in
        0.5
        *: ((2. *: p1)
           +: (t *: (p2 -: p0))
           +: ((t *. t) *: ((2. *: p0) -: (5. *: p1) +: (4. *: p2) -: p3))
           +: ((t *. t *. t) *: ((3. *: p1) -: p0 -: (3. *: p2) +: p3))))
  in
  List.concat (List.init (n - 1) segment) @ [ a.(n - 1) ]

(* Unit normals (to the right of travel) along a sampled path. *)
let normals a =
  let n = Array.length a in
  Array.mapi
    (fun i _ ->
      let d = a.(min (n - 1) (i + 1)) -: a.(max 0 (i - 1)) in
      let l = Float.max 1e-6 (Float.hypot d.x d.y) in
      v (-.d.y /. l) (d.x /. l))
    a

let heading_at a i =
  let n = Array.length a in
  angle_of (a.(min (n - 1) (i + 1)) -: a.(max 0 (i - 1)))

(* The outline of a path drawn with a pen whose width varies along it. *)
let ribbon pts width =
  let a = Array.of_list pts in
  let ns = normals a in
  let n = Array.length a in
  let t i = if n = 1 then 0. else float i /. float (n - 1) in
  let side k = List.init n (fun i -> a.(i) +: ((k *. width (t i) /. 2.) *: ns.(i))) in
  side 1. @ List.rev (side (-1.))

(* Cumulative arc length along a sampled path. *)
let arc_lengths a =
  let n = Array.length a in
  let rec go i acc s =
    if i >= n then Array.of_list (List.rev acc)
    else
      let s = s +. Float.hypot (a.(i).x -. a.(i - 1).x) (a.(i).y -. a.(i - 1).y) in
      go (i + 1) (s :: acc) s
  in
  if n = 0 then [||] else go 1 [ 0. ] 0.

let index_at_length lengths s =
  let n = Array.length lengths in
  let rec go i = if i >= n - 1 || lengths.(i) >= s then i else go (i + 1) in
  go 0

(** {1 SVG} *)

let num x =
  let s = Printf.sprintf "%.1f" x in
  if s = "-0.0" then "0" else s

let pt p = num p.x ^ "," ^ num p.y

let line_d = function
  | [] -> ""
  | p :: ps -> "M" ^ pt p ^ String.concat "" (List.map (fun p -> "L" ^ pt p) ps)

let closed_d pts = line_d pts ^ "Z"
let seg_d a b = "M" ^ pt a ^ "L" ^ pt b

let cubic_d start segs =
  "M" ^ pt start
  ^ String.concat ""
      (List.map (fun (a, b, c) -> "C" ^ pt a ^ " " ^ pt b ^ " " ^ pt c) segs)
  ^ "Z"

let attrs kvs = String.concat "" (List.map (fun (k, v) -> Printf.sprintf " %s=\"%s\"" k v) kvs)
let path d a = Printf.sprintf "<path d=\"%s\"%s/>" d (attrs a)

let circle c r a =
  Printf.sprintf "<circle cx=\"%s\" cy=\"%s\" r=\"%s\"%s/>" (num c.x) (num c.y) (num r) (attrs a)

let ellipse c rx ry rot a =
  Printf.sprintf "<ellipse rx=\"%s\" ry=\"%s\" transform=\"translate(%s) rotate(%s)\"%s/>" (num rx)
    (num ry) (pt c) (num (deg rot)) (attrs a)

let g a children = Printf.sprintf "<g%s>%s</g>" (attrs a) (String.concat "" children)
let fill c = ("fill", c)
let stroke c w = [ ("stroke", c); ("stroke-width", num w) ]
let round = [ ("stroke-linecap", "round"); ("stroke-linejoin", "round") ]
let no_fill = fill "none"

(** {1 Palettes} *)

type palette = {
  name : string;
  ink : string; (* outlines, stems and the neuron *)
  leaf : string; (* the shaded half of a leaf *)
  leaf_lit : string; (* the lit half *)
  paper : string; (* highlights: veins, streaks, lace *)
  bloom : string * string * string; (* main flowers: dark, mid, light *)
  bloom2 : string * string * string; (* small flowers *)
  berry : string * string * string;
  gold : string;
  eye : string; (* centres of the small flowers *)
}

(* Forest green, ochre and brick red, as in risograph prints. *)
let forest =
  {
    name = "forest";
    ink = "#1c3326";
    leaf = "#24503a";
    leaf_lit = "#5b9467";
    paper = "#fbf3da";
    bloom = ("#76201a", "#b8392c", "#eb9a7c");
    bloom2 = ("#8a6418", "#c99a3c", "#f0d898");
    berry = ("#6a1a13", "#b8392c", "#ef9b85");
    gold = "#cc9b3d";
    eye = "#b8392c";
  }

(* Blue ink with gold, as in engraved borders. *)
let indigo =
  {
    name = "indigo";
    ink = "#162d68";
    leaf = "#1f4597";
    leaf_lit = "#5583d4";
    paper = "#fbfaf3";
    bloom = ("#15337a", "#3a6cc9", "#bcd0f3");
    bloom2 = ("#2b55aa", "#7aa0e3", "#e3ebfb");
    berry = ("#122a66", "#2f5fb8", "#a3bff0");
    gold = "#c58b2e";
    eye = "#c58b2e";
  }

let gradients pal =
  let stops (d, m, l) =
    Printf.sprintf
      "<stop offset=\"0\" stop-color=\"%s\"/><stop offset=\".5\" stop-color=\"%s\"/><stop \
       offset=\"1\" stop-color=\"%s\"/>"
      d m l
  in
  let rev (d, m, l) = (l, m, d) in
  String.concat ""
    [
      Printf.sprintf "<linearGradient id=\"pa\">%s</linearGradient>" (stops pal.bloom);
      Printf.sprintf "<linearGradient id=\"pb\">%s</linearGradient>" (stops pal.bloom2);
      Printf.sprintf "<radialGradient id=\"berry\" cx=\".35\" cy=\".35\" r=\".75\">%s</radialGradient>"
        (stops (rev pal.berry));
      Printf.sprintf "<radialGradient id=\"soma\" cx=\".4\" cy=\".55\" r=\".7\">%s</radialGradient>"
        (stops (rev pal.bloom));
    ]

(* Risograph-style speckle: knock small holes in the ink where the noise is
   high. *)
let grain_filter ~w ~h =
  Printf.sprintf
    "<filter id=\"grain\" filterUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"%d\" \
     height=\"%d\"><feTurbulence type=\"fractalNoise\" baseFrequency=\".8\" numOctaves=\"2\" \
     seed=\"7\" result=\"n\"/><feColorMatrix in=\"n\" type=\"matrix\" values=\"0 0 0 0 0 0 0 0 0 0 \
     0 0 0 0 0 -2.6 0 0 0 2.35\" result=\"a\"/><feComposite in=\"SourceGraphic\" in2=\"a\" \
     operator=\"in\"/></filter>"
    w h

(** {1 Botanical parts} *)

(* A leaf along a gently bending midrib: the lit half pale, the other half
   dark, with pale veins, as in an engraving. *)
let leaf pal ~at ~heading ~length ?(width = 0.36) ?(bend = 0.25) () =
  let n = 16 in
  let spine =
    Array.of_list
      (grow ~step:(length /. float n) ~n at heading (fun _ _ i ->
           heading +. (bend *. float i /. float n)))
  in
  let ns = normals spine in
  let h i =
    let t = float i /. float n in
    width *. length *. 0.5 *. (t ** 0.6) *. (1. -. t) /. 0.347
  in
  let edge k = List.init (n + 1) (fun i -> spine.(i) +: ((k *. h i) *: ns.(i))) in
  let midrib = Array.to_list spine in
  let lit = if ns.(n / 2).y < 0. then 1. else -1. in
  let half k colour = path (closed_d (midrib @ List.rev (edge k))) [ fill colour ] in
  let nv = max 3 (int_of_float (length /. 7.)) in
  let veins =
    List.init nv (fun j ->
        let i0 = int_of_float (float n *. (0.08 +. (0.8 *. float (j + 1) /. float (nv + 1)))) in
        let i1 = min n (i0 + 2) in
        List.map
          (fun k -> seg_d spine.(i0) (spine.(i1) +: ((k *. h i1 *. 0.8) *: ns.(i1))))
          [ 1.; -1. ])
    |> List.concat |> String.concat ""
  in
  [
    half lit pal.leaf_lit;
    half (-.lit) pal.leaf;
    path (closed_d (edge 1. @ List.rev (edge (-1.)))) ([ no_fill ] @ stroke pal.ink 0.55 @ round);
    path (line_d (List.filteri (fun i _ -> i <= n - 2) midrib)) ([ no_fill ] @ stroke pal.paper 0.6);
    path veins ([ no_fill; ("opacity", ".8") ] @ stroke pal.paper 0.45);
  ]

(* A small plain leaflet, for sprigs and the tree's foliage. *)
let leaflet pal ~at ~heading ~length ~colour =
  let w = length *. 0.42 in
  let p x y = at +: ((x *. length) *: dir heading) +: ((y *. w) *: dir (heading +. (pi /. 2.))) in
  path
    (cubic_d at [ (p 0.3 (-0.6), p 0.75 (-0.5), p 1. 0.); (p 0.75 0.5, p 0.3 0.6, at) ])
    ([ fill colour ] @ stroke pal.ink 0.4)

(* An open five-petalled flower seen from above (and a little to the side),
   petals shaded dark to light from the centre and streaked like an
   engraving. *)
let flower5 pal r ~grad ~at ~radius ~tilt ~squash =
  let petal k =
    let rk = Rng.nth r k in
    let th = (float k *. 72.) +. Rng.range rk (-7.) 7. in
    let l = radius *. Rng.range (Rng.key rk "l") 0.86 1.04 and w = radius *. 0.52 in
    let p x y = v (x *. l) (y *. w) in
    let shape =
      cubic_d (v 0. 0.)
        [
          (p 0.3 (-0.55), p 0.62 (-1.), p 0.9 (-0.75));
          (p 1.02 (-0.55), p 1.04 (-0.12), p 0.95 0.);
          (p 1.04 0.12, p 1.02 0.55, p 0.9 0.75);
          (p 0.62 1., p 0.3 0.55, p 0. 0.);
        ]
    in
    let streaks =
      List.init 7 (fun j ->
          let y = float (j - 3) in
          seg_d (p 0.2 (y *. 0.07)) (p (0.74 +. (0.06 *. float (j mod 2))) (y *. 0.21)))
      |> String.concat ""
    in
    let base =
      List.init 5 (fun j ->
          let y = float (j - 2) in
          seg_d (p 0.06 (y *. 0.04)) (p 0.36 (y *. 0.16)))
      |> String.concat ""
    in
    g
      [ ("transform", Printf.sprintf "rotate(%s)" (num th)) ]
      [
        path shape ([ fill (Printf.sprintf "url(#%s)" grad) ] @ stroke pal.ink 0.6 @ round);
        path streaks ([ no_fill; ("opacity", ".75") ] @ stroke pal.paper 0.45);
        path base ([ no_fill ] @ stroke pal.ink 0.45);
      ]
  in
  let stamens =
    List.init 12 (fun k ->
        let a = (float k *. 2. *. pi /. 12.) +. 0.2 in
        let e = (radius *. 0.3) *: dir a in
        [
          path (seg_d ((radius *. 0.08) *: dir a) e) (stroke pal.ink 0.45);
          circle e 1.25 ([ fill pal.gold ] @ stroke pal.ink 0.35);
        ])
    |> List.concat
  in
  g
    [
      ( "transform",
        Printf.sprintf "translate(%s) rotate(%s) scale(1 %s)" (pt at) (num tilt) (num squash) );
    ]
    (List.init 5 petal @ stamens
    @ [
        circle (v 0. 0.) (radius *. 0.12) ([ fill pal.gold ] @ stroke pal.ink 0.5);
        circle (v 0. 0.) (radius *. 0.045) [ fill pal.ink ];
      ])

(* A trumpet-shaped flower seen from the side, opening along [heading]. *)
let bell pal ~grad ~at ~heading ~length =
  let (dark, _, _) = pal.bloom in
  let petal ~a ~l ~colour =
    let w = l *. 0.34 in
    let p x y = v (x *. l) (y *. w) in
    g
      [ ("transform", Printf.sprintf "rotate(%s)" (num a)) ]
      [
        path
          (cubic_d (v 0. 0.) [ (p 0.35 (-0.5), p 0.72 (-0.55), p 1. 0.); (p 0.72 0.55, p 0.35 0.5, v 0. 0.) ])
          ([ fill colour ] @ stroke pal.ink 0.55 @ round);
        path (seg_d (p 0.15 0.) (p 0.8 0.)) ([ ("opacity", ".7") ] @ stroke pal.paper 0.45);
      ]
  in
  let url = Printf.sprintf "url(#%s)" grad in
  let stamens =
    List.map
      (fun y ->
        let e = v (length *. 0.95) (y *. length *. 0.12) in
        [ path (seg_d (v 2. 0.) e) (stroke pal.ink 0.4); circle e 1.1 ([ fill pal.gold ] @ stroke pal.ink 0.3) ])
      [ -1.; 0.2; 1. ]
    |> List.concat
  in
  g
    [ ("transform", Printf.sprintf "translate(%s) rotate(%s)" (pt at) (num (deg heading))) ]
    [
      g
        [ ("transform", "translate(5 0)") ]
        ([
           petal ~a:(-44.) ~l:(length *. 0.8) ~colour:dark;
           petal ~a:44. ~l:(length *. 0.8) ~colour:dark;
           petal ~a:(-22.) ~l:length ~colour:url;
           petal ~a:22. ~l:length ~colour:url;
           petal ~a:0. ~l:(length *. 1.05) ~colour:url;
         ]
        @ stamens);
      path "M0,-2.2L6,-4.2L4.6,0L6,4.2L0,2.2Z" ([ fill pal.leaf ] @ stroke pal.ink 0.45 @ round);
    ]

(* Queen Anne's lace: a flat-topped umbel of umbellets of tiny florets. *)
let umbel pal r ~at ~heading ~length =
  let n = 11 and spread = 0.95 in
  let rays =
    List.init n (fun i ->
        let f = (float i /. float (n - 1)) -. 0.5 in
        let a = heading +. (f *. 2. *. spread) in
        let l =
          length *. (0.5 +. (0.5 /. cos (f *. 2. *. spread))) *. Rng.range (Rng.nth r i) 0.92 1.04
        in
        at +: (l *: dir a))
  in
  let umbellet i e =
    let m = 7 in
    List.init m (fun j ->
        let f = (float j /. float (m - 1)) -. 0.5 in
        let a = heading +. (f *. 2.2) in
        let l = length *. 0.16 *. (0.55 +. (0.45 /. cos (f *. 1.1))) *. Rng.range (Rng.nth (Rng.nth r i) j) 0.85 1.1 in
        (e, e +: (l *: dir a)))
  in
  let florets = List.concat (List.mapi umbellet rays) in
  let bracts =
    List.map
      (fun s ->
        line_d
          (curve ~step:1.5 at (length *. 0.3) (fun d -> heading +. pi -. (s *. (0.5 +. (d *. 0.06))))))
      [ -1.; 1.; -0.4 ]
  in
  [
    path (String.concat "" (List.map (fun e -> seg_d at e) rays)) ([ no_fill ] @ stroke pal.leaf 0.5 @ round);
    path (String.concat "" (List.map (fun (a, b) -> seg_d a b) florets)) ([ no_fill ] @ stroke pal.leaf 0.4);
    path (String.concat "" bracts) ([ no_fill ] @ stroke pal.ink 0.45 @ round);
  ]
  @ List.mapi
      (fun i (_, e) ->
        circle e (if i mod 3 = 0 then 1.05 else 1.3) ([ fill pal.paper ] @ stroke pal.ink 0.35))
      florets

(* Forget-me-nots: tiny flowers along a stalk that curls up at the tip, buds
   at the end. *)
let cyme pal r ~at ~heading ~length ~curl =
  let (dark, mid, light) = pal.bloom2 in
  let n = max 4 (int_of_float (length /. 2.)) in
  let stalk =
    Array.of_list
      (grow ~step:2. ~n at heading (fun _ _ i ->
           let t = float i /. float n in
           heading +. (curl *. 2.8 *. (t ** 2.2))))
  in
  let ns = normals stalk in
  let flower c size =
    List.init 5 (fun k ->
        let a = (float k *. 2. *. pi /. 5.) +. Rng.range r 0. 1. in
        circle (c +: ((size *. 0.55) *: dir a)) (size *. 0.5) ([ fill light ] @ stroke dark 0.4))
    @ [ circle c (size *. 0.24) [ fill pal.eye ] ]
  in
  let items =
    List.init (n / 5) (fun k ->
        let i = (n * 3 / 10) + (k * 5) in
        if i > n then []
        else
          let t = float i /. float n in
          let side = if k mod 2 = 0 then 1. else -1. in
          let c = stalk.(i) +: ((side *. lerp 6. 3. t) *: ns.(i)) in
          path (seg_d stalk.(i) c) (stroke pal.ink 0.45)
          ::
          (if t > 0.82 then
             [ ellipse c 2.3 1.5 (heading_at stalk i) ([ fill mid ] @ stroke pal.ink 0.4) ]
           else flower c (lerp 4.4 2.4 t)))
    |> List.concat
  in
  path (line_d (Array.to_list stalk)) ([ no_fill ] @ stroke pal.ink 0.8 @ round) :: items

(* A small cluster of berries on thin stalks. *)
let berries pal r ~at ~heading =
  List.init
    (3 + Hashtbl.hash r mod 3)
    (fun i ->
      let ri = Rng.nth r i in
      let a = heading +. Rng.range (Rng.key ri "a") (-0.9) 0.9 in
      let e = at +: (Rng.range (Rng.key ri "l") 6. 13. *: dir a) in
      let rad = Rng.range (Rng.key ri "r") 2.4 3.5 in
      [
        path (seg_d at e) (stroke pal.ink 0.6);
        circle e rad ([ fill "url(#berry)" ] @ stroke pal.ink 0.45);
        circle (e +: v (-.rad *. 0.35) (-.rad *. 0.35)) (rad *. 0.22) [ fill pal.paper ];
      ])
  |> List.concat

(* A tendril: a thin stalk winding into a tightening spiral. *)
let tendril pal ~at ~heading ~length ~curl =
  path
    (line_d
       (curve ~step:1.2 at length (fun s -> heading +. (curl *. ((s /. length) ** 1.7) *. 4.6 *. pi))))
    ([ no_fill ] @ stroke pal.ink 0.7 @ round)

(* A sprig of paired leaflets, shrinking to the tip. *)
let sprig pal ~at ~heading ~length ~bend =
  let stalk = Array.of_list (curve ~step:2. at length (fun s -> heading +. (bend *. s /. length))) in
  let n = Array.length stalk - 1 in
  let leaves =
    List.init (n / 3) (fun k ->
        let i = 2 + (k * 3) in
        if i > n then []
        else
          let t = float i /. float n in
          let a = heading_at stalk i in
          let l = lerp 8. 3.5 t in
          List.map
            (fun s ->
              leaflet pal ~at:stalk.(i) ~heading:(a +. (s *. 0.8)) ~length:l
                ~colour:(if s > 0. then pal.leaf_lit else pal.leaf))
            [ -1.; 1. ])
    |> List.concat
  in
  path (line_d (Array.to_list stalk)) ([ no_fill ] @ stroke pal.ink 0.7 @ round) :: leaves

(** {1 Branching crowns} *)

type arbor = {
  inside : v -> bool; (* where branches may grow *)
  focus : v; (* branches lean away from here *)
  steer : float;
  spread : float * float;
  taper : float;
  min_width : float;
  max_depth : int;
  shrink : float * float; (* each fork is this much shorter *)
}

type limb = { pts : v array; width : float; tip : bool }

(* A branch that grows, steering out from the focus, then forks; the result
   lists every limb of the subtree. *)
let rec limbs cfg r ~from ~heading ~length ~width ~depth =
  let n = max 2 (int_of_float (length /. 2.)) in
  let ph = Rng.range (Rng.key r "ph") 0. 6.28 in
  let raw =
    grow ~step:2. ~n from heading (fun p a i ->
        a
        +. (cfg.steer *. turn_to a (angle_of (p -: cfg.focus)))
        +. (0.07 *. sin ((float i *. 0.6) +. ph)))
  in
  let pts = List.take_while cfg.inside raw in
  let stopped = List.compare_lengths pts raw < 0 in
  let arr = Array.of_list pts in
  let k = Array.length arr in
  let children =
    if stopped || k < 2 || width *. cfg.taper < cfg.min_width || depth >= cfg.max_depth then []
    else
      let forks = if depth < 2 && Rng.chance (Rng.key r "three") 0.4 then 3 else 2 in
      let lo, hi = cfg.spread in
      List.init forks (fun i ->
          let ri = Rng.nth r i in
          let off = (float i -. (float (forks - 1) /. 2.)) *. Rng.range (Rng.key ri "spread") lo hi in
          limbs cfg ri ~from:arr.(k - 1)
            ~heading:(heading_at arr (k - 1) +. off)
            ~length:(length *. Rng.range (Rng.key ri "len") (fst cfg.shrink) (snd cfg.shrink))
            ~width:(width *. cfg.taper) ~depth:(depth + 1))
      |> List.concat
  in
  if k < 2 then children else { pts = arr; width; tip = children = [] } :: children

(* One path per pen width (to tenths), rather than one per limb. *)
let draw_limbs pal limbs =
  let pen l = Float.round (l.width *. 10.) in
  List.sort_uniq compare (List.map pen limbs)
  |> List.map (fun w ->
         let d = List.filter (fun l -> pen l = w) limbs |> List.map (fun l -> line_d (Array.to_list l.pts)) in
         path (String.concat "" d) ([ no_fill ] @ stroke pal.ink (w /. 10.) @ round))

(* Short wiggly side twigs along the thin limbs: the "spiny branchlets"
   that fill a Purkinje cell's arbor. *)
let twigs cfg r limbs =
  List.concat_map
    (fun (j, l) ->
      if l.width > 1.05 then []
      else
        let n = Array.length l.pts in
        List.init (n / 7) (fun k ->
            let i = 3 + (k * 7) in
            if i >= n then None
            else
              let rk = Rng.nth (Rng.nth r j) k in
              let side = if k mod 2 = 0 then 1. else -1. in
              let a = heading_at l.pts i +. (side *. Rng.range (Rng.key rk "a") 0.5 1.1) in
              let ph = Rng.range (Rng.key rk "ph") 0. 6.28 in
              let pts =
                curve ~step:1.5 l.pts.(i) (Rng.range (Rng.key rk "l") 4. 8.5) (fun s -> a +. (0.35 *. sin ((s *. 0.5) +. ph)))
                |> List.take_while cfg.inside
              in
              if List.length pts < 2 then None else Some { pts = Array.of_list pts; width = 0.5; tip = true })
        |> List.filter_map Fun.id)
    (List.mapi (fun j l -> (j, l)) limbs)

(* Dendritic spines: tiny ticks along the thin limbs and twigs. *)
let spines pal r limbs =
  let ticks =
    List.concat_map
      (fun (j, l) ->
        if l.width > 1.0 then []
        else
          List.init (Array.length l.pts) (fun i ->
              if i = 0 || i mod 3 <> 0 then ""
              else
                let ri = Rng.nth (Rng.nth r j) i in
                let side = if i mod 6 = 0 then 1. else -1. in
                let a = heading_at l.pts i +. (side *. Rng.range (Rng.key ri "a") 1.0 1.9) in
                seg_d l.pts.(i) (l.pts.(i) +: (Rng.range (Rng.key ri "l") 1.1 2.3 *: dir a))))
      (List.mapi (fun j l -> (j, l)) limbs)
  in
  path (String.concat "" ticks) ([ no_fill ] @ stroke pal.ink 0.42 @ round)

let in_ellipse c rx ry p =
  let dx = (p.x -. c.x) /. rx and dy = (p.y -. c.y) /. ry in
  (dx *. dx) +. (dy *. dy) < 1.


(** {1 Composition} *)

type crown = Purkinje | Tree | Bloom

let width = 340.
let height = 1000.
let cx = 170.

(* Where the main stem ends and the crown begins. *)
let crown_base = function Purkinje -> 338. | Tree -> 372. | Bloom -> 130.

(* Where the stem meets the crown: off centre for the Purkinje cell, whose
   soma sits right of the middle, with its long limb sweeping left. *)
let stem_top_x = function Purkinje -> cx +. 28. | Tree | Bloom -> cx

(* Where flowers and leaves stop on the stem (above: axon, trunk or bare stem). *)
let plant_top = function Purkinje -> 500. | Tree -> 430. | Bloom -> 250.

(* The main stem: a spline from below the bottom edge up to the crown,
   swinging from side to side, less so near the top. *)
let main_stem r crown =
  let top = crown_base crown in
  let ys = List.filter (fun y -> y > top +. 30.) [ 1040.; 950.; 860.; 770.; 680.; 595.; 515.; 440. ] @ [ top ] in
  let k = List.length ys in
  spline
    (List.mapi
       (fun i y ->
         let t = float i /. float (k - 1) in
         let amp = 74. *. ((1. -. t) ** 0.6) in
         let jit = Rng.range (Rng.nth (Rng.key r "stem") i) (-8.) 8. in
         if i = k - 1 then v (stem_top_x crown) y else v (cx +. (amp *. sin (1.9 +. (1.25 *. float i))) +. jit) y)
       ys)

type node = { back : string list; front : string list }

(* One node of the stem: a leaf on one side; on the other, a branch with a
   leaf and a flower (or umbel, or berries...) at its end; sometimes a
   tendril. *)
let node pal r ~i ~at ~headroom =
  let base_side = if i mod 2 = 0 then 1. else -1. in
  let side = if at.x < 100. then 1. else if at.x > width -. 100. then -1. else base_side in
  let room s = (if s > 0. then width -. at.x else at.x) -. 22. in
  let rb = Rng.key r "branch" in
  let a0 = up +. (side *. Rng.range (Rng.key rb "a") 0.7 1.1) in
  let len = Float.min (Rng.range (Rng.key rb "l") 52. 100.) (room side *. 1.15) in
  let ph = Rng.range (Rng.key rb "ph") 0. 6.28 in
  let pts =
    Array.of_list
      (curve at len (fun s -> a0 -. (side *. 0.008 *. s) +. (0.1 *. sin ((s /. 13.) +. ph))))
  in
  let n = Array.length pts in
  let tip = pts.(n - 1) and tip_heading = heading_at pts (n - 1) in
  let edge = Float.min (tip.x -. 4.) (width -. 4. -. tip.x) in
  let rf = Rng.key r "flower" in
  let tilt lo hi = Rng.range (Rng.key rf "t") lo hi in
  let flower =
    (* near the top, nothing tall (umbels, cymes, sprigs) that would reach the crown *)
    let k = i mod 8 in
    match if headroom < 70. then 4 else if headroom < 130. && List.mem k [ 1; 2; 6 ] then 5 else k with
    | 0 ->
        let radius = Float.min (Rng.range rf 22. 27.) edge in
        [ flower5 pal rf ~grad:"pa" ~at:tip ~radius ~tilt:(tilt (-30.) 30.) ~squash:0.8 ]
    | 1 -> umbel pal rf ~at:tip ~heading:(lerp tip_heading up 0.6) ~length:(Float.min 30. edge)
    | 2 -> cyme pal rf ~at:tip ~heading:tip_heading ~length:46. ~curl:side
    | 3 -> [ bell pal ~grad:"pa" ~at:tip ~heading:tip_heading ~length:23. ]
    | 4 -> berries pal rf ~at:tip ~heading:tip_heading
    | 5 ->
        let radius = Float.min 18. edge in
        [ flower5 pal rf ~grad:"pb" ~at:tip ~radius ~tilt:(tilt (-40.) 40.) ~squash:0.75 ]
    | 6 -> sprig pal ~at:tip ~heading:tip_heading ~length:38. ~bend:(-.side *. 0.5)
    | _ -> [ bell pal ~grad:"pb" ~at:tip ~heading:tip_heading ~length:20. ]
  in
  let branch = path (closed_d (ribbon (Array.to_list pts) (fun t -> lerp 2.6 1.0 t))) [ fill pal.ink ] in
  let branch_leaf =
    leaf pal ~at:pts.(n / 2)
      ~heading:(heading_at pts (n / 2) +. (side *. 0.9))
      ~length:(Rng.range (Rng.key r "bl") 20. 28.)
      ~bend:(-.side *. 0.3) ()
  in
  let rl = Rng.key r "leaf" in
  let node_leaf =
    leaf pal ~at
      ~heading:(up -. (side *. Rng.range (Rng.key rl "a") 0.8 1.2))
      ~length:(Float.min (Rng.range (Rng.key rl "l") 34. 48.) (room (-.side) *. 0.85))
      ~bend:(side *. 0.35) ()
  in
  let low_leaf =
    if Rng.chance (Rng.key rl "low") 0.5 then
      leaf pal ~at
        ~heading:(up +. (side *. Rng.range (Rng.key rl "b") 1.6 2.0))
        ~length:(Rng.range (Rng.key rl "m") 20. 28.)
        ~bend:(-.side *. 0.4) ()
    else []
  in
  let tendrils =
    if i mod 3 = 2 then [ tendril pal ~at ~heading:(up -. (side *. 0.4)) ~length:32. ~curl:(-.side) ]
    else []
  in
  {
    back = node_leaf @ low_leaf @ (branch :: branch_leaf) @ tendrils;
    front = flower;
  }

(* The second, thinner vine twining around the first. *)
let second_vine pal r =
  let pts =
    spline
      [
        v (cx +. 50.) 1040.; v (cx -. 30.) 945.; v (cx +. 70.) 850.; v (cx -. 50.) 750.;
        v (cx +. 40.) 660.; v (cx -. 10.) 600.;
      ]
  in
  let a = Array.of_list pts in
  let n = Array.length a in
  let stem = path (closed_d (ribbon pts (fun t -> lerp 3. 1.0 t))) [ fill pal.ink ] in
  let leaves =
    List.init (n / 30) (fun k ->
        let i = 18 + (k * 30) in
        if i >= n - 8 then []
        else
          let side = if k mod 2 = 0 then -1. else 1. in
          leaf pal ~at:a.(i)
            ~heading:(heading_at a i +. (side *. 0.9))
            ~length:(Rng.range (Rng.nth (Rng.key r "v2") k) 18. 26.)
            ~bend:(-.side *. 0.3) ())
    |> List.concat
  in
  {
    back = stem :: leaves;
    front = cyme pal (Rng.key r "v2c") ~at:a.(n - 1) ~heading:(heading_at a (n - 1)) ~length:36. ~curl:(-1.);
  }

(** {2 Space colonization}

    Runions et al.'s algorithm for trees that fill a region evenly: scatter
    attraction points over it, then grow a step at a time toward them. Each
    point pulls on the nearest node within [influence] and is used up once
    a node comes within [kill]; [lift] adds an upward pull and [wobble n]
    turns node [n]'s next step a little. Growth starts from the [seed] tree. *)

module Cells = Map.Make (struct
  type t = int * int

  let compare = compare
end)

module Ids = Map.Make (Int)

type sc = { id : int; pos : v; parent : int (* -1 for the root *) }

let unit_v a =
  let l = Float.hypot a.x a.y in
  if l < 1e-9 then a else (1. /. l) *: a

let dist a b = Float.hypot (a.x -. b.x) (a.y -. b.y)

let colonize ~attractors ~seed ~influence ~kill ~step ~lift ~wobble ~rounds =
  let cell p = (int_of_float (Float.floor (p.x /. influence)), int_of_float (Float.floor (p.y /. influence))) in
  let add grid n = Cells.update (cell n.pos) (fun l -> Some (n :: Option.value l ~default:[])) grid in
  let nearest grid p =
    let ci, cj = cell p in
    List.concat_map (fun di -> List.map (fun dj -> (ci + di, cj + dj)) [ -1; 0; 1 ]) [ -1; 0; 1 ]
    |> List.concat_map (fun c -> Option.value (Cells.find_opt c grid) ~default:[])
    |> List.fold_left
         (fun best n ->
           let d = dist n.pos p in
           match best with
           | Some (_, bd) when bd <= d -> best
           | _ when d < influence -> Some (n, d)
           | _ -> best)
         None
  in
  let rec go k nodes grid next attractors =
    let pulls, alive =
      List.fold_left
        (fun (pulls, alive) a ->
          match nearest grid a with
          | Some (_, d) when d < kill -> (pulls, alive)
          | Some (n, _) ->
              (Ids.update n.id (fun l -> Some (n, a :: Option.fold ~none:[] ~some:snd l)) pulls, a :: alive)
          | None -> (pulls, a :: alive))
        (Ids.empty, []) attractors
    in
    let grown, next =
      Ids.fold
        (fun _ (n, pts) (acc, next) ->
          let pull = List.fold_left (fun s a -> s +: unit_v (a -: n.pos)) (v 0. 0.) pts in
          let d = unit_v (unit_v pull +: v 0. (-.lift)) in
          let p = n.pos +: (step *: dir (angle_of d +. wobble n)) in
          match nearest grid p with
          | Some (_, d) when d < step *. 0.5 -> (acc, next)
          | _ -> ({ id = next; pos = p; parent = n.id } :: acc, next + 1))
        pulls ([], next)
    in
    if grown = [] || k >= rounds then nodes
    else go (k + 1) (grown @ nodes) (List.fold_left add grid grown) next alive
  in
  let next = 1 + List.fold_left (fun m n -> max m n.id) 0 seed in
  go 0 seed (List.fold_left add Cells.empty seed) next attractors

(* How many tips each node feeds. Children are newer than their parents,
   so one pass from the newest node back suffices. *)
let tips nodes =
  List.sort (fun a b -> compare b.id a.id) nodes
  |> List.fold_left
       (fun m n ->
         let t = match Ids.find_opt n.id m with None -> 1 | Some s -> s in
         let m = Ids.add n.id t m in
         if n.parent < 0 then m else Ids.update n.parent (fun s -> Some (Option.value s ~default:0 + t)) m)
       Ids.empty

(* Soften the zigzags: pull each node a little toward its parent and its
   children. *)
let smooth nodes =
  let by_id = List.fold_left (fun m n -> Ids.add n.id n m) Ids.empty nodes in
  let kids =
    List.fold_left
      (fun m n -> if n.parent < 0 then m else Ids.update n.parent (fun l -> Some (n :: Option.value l ~default:[])) m)
      Ids.empty nodes
  in
  List.map
    (fun n ->
      match (Ids.find_opt n.parent by_id, Ids.find_opt n.id kids) with
      | Some p, Some (c :: _) -> { n with pos = (0.5 *: n.pos) +: (0.25 *: p.pos) +: (0.25 *: c.pos) }
      | _ -> n)
    nodes

let soma_at c =
  cubic_d (c +: v (-10.) 0.)
    [
      (c +: v (-10.5) (-8.), c +: v (-4.) (-10.), c +: v (-2.6) (-15.));
      (c +: v 0. (-16.), c +: v 0. (-16.), c +: v 2.6 (-15.));
      (c +: v 4. (-10.), c +: v 10.5 (-8.), c +: v 10. 0.);
      (c +: v 9.5 10., c +: v (-9.5) 10., c +: v (-10.) 0.);
    ]

(* A Purkinje cell as Ramón y Cajal drew them: a dark soma at the bottom,
   thick limbs spreading sideways from it, and from those, wavy dendrites
   rising to fill a square block, fuzzy with spiny branchlets. *)
let purkinje pal r =
  let soma = v (stem_top_x Purkinje) (crown_base Purkinje -. 8.) in
  let root = { id = 0; pos = soma +: v 0. (-22.); parent = -1 } in
  (* the main limbs, drawn by hand and lopsided as in Cajal's drawing: one
     long heavy limb sweeping far out to the left, with limbs of its own
     rising from it, and shorter, uneven ones to the right; the rest grows
     from these *)
  let chain parent first heading length =
    curve ~step:3. parent.pos length heading
    |> List.tl
    |> List.mapi (fun i p -> { id = first + i; pos = p; parent = (if i = 0 then parent.id else first + i - 1) })
  in
  let at l i = List.nth l (min (List.length l - 1) i) in
  let wave ph s = (0.1 *. sin ((s /. 11.) +. ph)) +. (0.05 *. sin ((s /. 4.3) +. (2. *. ph))) in
  let left = chain root 1 (fun s -> up -. 1.36 +. (0.0032 *. s) +. wave 0.3 s) 180. in
  let left_up = chain (at left 22) 100 (fun s -> up -. 0.35 +. (0.004 *. s) +. wave 1.7 s) 58. in
  let left_far = chain (at left 44) 150 (fun s -> up -. 0.15 +. wave 2.2 s) 30. in
  let right = chain root 200 (fun s -> up +. 0.95 -. (0.006 *. s) -. wave 0.9 s) 78. in
  let middle = chain root 300 (fun s -> up +. 0.22 -. (0.003 *. s) +. wave 2.9 s) 42. in
  let middle_side = chain (at middle 9) 400 (fun s -> up -. 0.5 +. (0.008 *. s) +. wave 4.1 s) 30. in
  let seed = (root :: left) @ left_up @ left_far @ right @ middle @ middle_side in
  (* Where the dendrites go: denser in some patches than others, with small
     voids (where, in the cerebellum, other cells sit) that they grow
     around, and little below the main limbs. *)
  let ra = Rng.key r "attractors" in
  let ph k = Rng.range (Rng.nth (Rng.key ra "ph") k) 0. 6.28 in
  let density p =
    0.68
    +. (0.18 *. sin ((p.x /. 31.) +. ph 0) *. sin ((p.y /. 43.) +. ph 1))
    +. (0.14 *. sin (((p.x +. p.y) /. 23.) +. ph 2))
  in
  let voids =
    List.init 70 (fun k ->
        let rk = Rng.nth (Rng.key ra "voids") k in
        ( v (Rng.range (Rng.key rk "x") 10. (width -. 10.)) (Rng.range (Rng.key rk "y") 20. (soma.y -. 30.)),
          Rng.range (Rng.key rk "rx") 4. 10.,
          Rng.range (Rng.key rk "ry") 7. 17. ))
  in
  let attractors =
    List.init 7000 (fun k ->
        let rk = Rng.nth ra k in
        (v (Rng.range (Rng.key rk "x") 8. (width -. 8.)) (Rng.range (Rng.key rk "y") 14. (soma.y -. 4.)), Rng.unit (Rng.key rk "keep")))
    |> List.filter_map (fun (p, u) ->
           if
             u < density p
             && p.y < soma.y -. 16. -. (0.18 *. Float.abs (p.x -. soma.x))
             && not (List.exists (fun (c, rx, ry) -> in_ellipse c rx ry p) voids)
           then Some p
           else None)
  in
  let rw = Rng.key r "wobble" in
  let nodes =
    colonize ~attractors ~seed ~influence:24. ~kill:5. ~step:2.5 ~lift:0.55
      ~wobble:(fun n -> Rng.range (Rng.nth rw n.id) (-0.4) 0.4)
      ~rounds:500
    |> smooth
  in
  let tips = tips nodes in
  let tips_of n = Option.value (Ids.find_opt n.id tips) ~default:1 in
  let by_id = List.fold_left (fun m n -> Ids.add n.id n m) Ids.empty nodes in
  let kids =
    List.fold_left
      (fun m n -> if n.parent < 0 then m else Ids.update n.parent (fun l -> Some (n :: Option.value l ~default:[])) m)
      Ids.empty nodes
  in
  let kids_of n = Option.value (Ids.find_opt n.id kids) ~default:[] in
  (* pen width in tenths, after the pipe model: thick where many tips are fed *)
  let pen n = Float.round (10. *. Float.max 0.45 (Float.min 5.6 (0.36 *. (float (tips_of n) ** 0.55)))) in
  (* unbranched runs, each one polyline of one width (tips, and so the pen,
     only change where the tree forks) *)
  let rec runs from n =
    let rec follow n acc = match kids_of n with [ c ] -> follow c (c.pos :: acc) | _ -> (n, List.rev acc) in
    let last, pts = follow n [ n.pos ] in
    (pen n, line_d (from :: pts)) :: List.concat_map (runs last.pos) (kids_of last)
  in
  let segments = List.concat_map (runs root.pos) (kids_of root) in
  let limbs =
    List.sort_uniq compare (List.map fst segments)
    |> List.map (fun w ->
           let d = List.filter_map (fun (w', s) -> if w' = w then Some s else None) segments in
           path (String.concat "" d) ([ no_fill ] @ stroke pal.ink (w /. 10.) @ round))
  in
  let heading n =
    match Ids.find_opt n.parent by_id with Some p -> angle_of (n.pos -: p.pos) | None -> up
  in
  let thin = List.filter (fun n -> tips_of n <= 2) nodes in
  let rt = Rng.key r "twigs" in
  let twigs =
    List.filter_map
      (fun n ->
        let rn = Rng.nth rt n.id in
        if not (Rng.chance rn 0.22) then None
        else
          let side = if Rng.chance (Rng.key rn "s") 0.5 then 1. else -1. in
          let a = heading n +. (side *. Rng.range (Rng.key rn "a") 0.7 1.4) in
          let ph = Rng.range (Rng.key rn "ph") 0. 6.28 in
          Some (line_d (curve ~step:1.5 n.pos (Rng.range (Rng.key rn "l") 3. 7.) (fun s -> a +. (0.5 *. sin ((s *. 0.7) +. ph))))))
      thin
  in
  let rs = Rng.key r "spines" in
  let spines =
    List.filter_map
      (fun n ->
        let rn = Rng.nth rs n.id in
        if not (Rng.chance rn 0.5) then None
        else
          let side = if Rng.chance (Rng.key rn "s") 0.5 then 1. else -1. in
          let a = heading n +. (side *. Rng.range (Rng.key rn "a") 1.1 1.9) in
          Some (seg_d n.pos (n.pos +: (Rng.range (Rng.key rn "l") 1.2 2.2 *: dir a))))
      thin
  in
  let trunk =
    path
      (closed_d (ribbon (spline [ soma +: v 0. (-12.); soma +: v (-0.5) (-18.); root.pos ]) (fun t -> lerp 6. 5.4 t)))
      [ fill pal.ink ]
  in
  ( [],
    limbs
    @ [
        path (String.concat "" twigs) ([ no_fill ] @ stroke pal.ink 0.45 @ round);
        path (String.concat "" spines) ([ no_fill ] @ stroke pal.ink 0.4 @ round);
        trunk;
        path (soma_at soma) ([ fill pal.ink ] @ stroke pal.ink 0.6);
      ] )

(* The axon: from the soma down to where the vine takes over, sheathed in
   myelin with gaps (nodes of Ranvier), with one recurrent collateral. *)
let axon pal stem =
  let a = Array.of_list stem in
  let lengths = arc_lengths a in
  let total = lengths.(Array.length lengths - 1) in
  let between s0 s1 = List.filteri (fun i _ -> lengths.(i) >= s0 && lengths.(i) <= s1) stem in
  let internodes =
    List.init (int_of_float ((total -. 26.) /. 19.)) (fun k ->
        let s0 = 8. +. (float k *. 19.) in
        between s0 (s0 +. 15.))
    |> List.filter (fun l -> List.length l >= 2)
  in
  let sheath w c = List.map (fun l -> path (line_d l) ([ no_fill ] @ stroke c w @ round)) internodes in
  let j = index_at_length lengths (total *. 0.6) in
  let collateral = curve ~step:1.5 a.(j) 46. (fun s -> heading_at a j +. 2.2 +. (s *. 0.022)) in
  let boutons = List.filteri (fun i _ -> i > 0 && i mod 8 = 0) collateral in
  let _, _, myelin = pal.bloom2 in
  [ path (line_d stem) ([ no_fill ] @ stroke pal.ink 1.2 @ round) ]
  @ sheath 5.0 pal.ink @ sheath 3.0 myelin
  @ [ path (line_d collateral) ([ no_fill ] @ stroke pal.ink 0.6 @ round) ]
  @ List.map (fun p -> circle p 1.3 [ fill pal.ink ]) boutons

(* A tree: the stem thickens into a trunk that branches into a round,
   leafy crown hung with a few red fruit. *)
let tree pal r =
  let base = v cx (crown_base Tree) in
  let cfg =
    {
      inside = (fun p -> p.y > 14. && p.x > 10. && p.x < width -. 10. && in_ellipse (v cx 215.) 160. 178. p);
      focus = v cx 380.;
      steer = 0.045;
      spread = (0.42, 0.8);
      taper = 0.74;
      min_width = 0.6;
      max_depth = 8;
      shrink = (0.74, 0.9);
    }
  in
  let trunk_pts = spline [ base; base +: v (-6.) (-40.); base +: v 4. (-80.) ] in
  let trunk_top = List.nth trunk_pts (List.length trunk_pts - 1) in
  let trunk = path (closed_d (ribbon trunk_pts (fun t -> lerp 7. 4.6 t))) [ fill pal.ink ] in
  let rl = Rng.key r "limbs" in
  let main =
    List.init 3 (fun i ->
        let ri = Rng.nth rl i in
        limbs cfg ri ~from:trunk_top ~heading:(up +. (float (i - 1) *. 0.75)) ~length:44. ~width:4.2 ~depth:0)
    |> List.concat
  in
  let rf = Rng.key r "foliage" in
  let foliage =
    List.concat_map
      (fun (j, l) ->
        if l.width > 2.2 then []
        else
          let n = Array.length l.pts in
          List.init (n / 6) (fun k ->
              let i = min (n - 1) (2 + (k * 6)) in
              let rk = Rng.nth (Rng.nth rf j) k in
              List.init 3 (fun m ->
                  let a = heading_at l.pts i +. (float (m - 1) *. 1.1) +. Rng.range (Rng.nth rk m) (-0.3) 0.3 in
                  leaflet pal ~at:l.pts.(i) ~heading:a
                    ~length:(Rng.range (Rng.key (Rng.nth rk m) "l") 7. 11.)
                    ~colour:(if (j + k + m) mod 2 = 0 then pal.leaf else pal.leaf_lit)))
          |> List.concat)
      (List.mapi (fun j l -> (j, l)) main)
  in
  let fruit =
    List.filteri (fun j l -> l.tip && j mod 6 = 1) main
    |> List.concat_map (fun l ->
           let e = l.pts.(Array.length l.pts - 1) in
           [ circle e 3.2 ([ fill "url(#berry)" ] @ stroke pal.ink 0.45); circle (e +: v (-1.) (-1.)) 0.7 [ fill pal.paper ] ])
  in
  ([], (trunk :: draw_limbs pal main) @ foliage @ fruit)

(* One big open flower with two buds, as at the head of an engraved border. *)
let bloom pal r =
  let c = v cx (crown_base Bloom) in
  let bud at heading =
    let p x y = at +: ((x *. 18.) *: dir heading) +: ((y *. 8.) *: dir (heading +. (pi /. 2.))) in
    [
      path (cubic_d at [ (p 0.2 (-1.), p 0.8 (-0.8), p 1. 0.); (p 0.8 0.8, p 0.2 1., at) ]) ([ fill "url(#pa)" ] @ stroke pal.ink 0.55);
      path (seg_d (p 0.1 0.) (p 0.85 0.)) ([ ("opacity", ".7") ] @ stroke pal.paper 0.45);
      path (cubic_d at [ (p 0.15 (-0.9), p 0.3 (-0.7), p 0.4 (-0.3)); (p 0.3 0., p 0.3 0., at) ]) ([ fill pal.leaf ] @ stroke pal.ink 0.4);
    ]
  in
  let stalk from a l = Array.of_list (curve from l (fun s -> a +. (s *. 0.012))) in
  let s1 = stalk (c +: v 4. 84.) (up +. 0.95) 60. and s2 = stalk (c +: v (-3.) 108.) (up -. 1.0) 54. in
  let stalk_path s = path (line_d (Array.to_list s)) ([ no_fill ] @ stroke pal.ink 1.3 @ round) in
  let last s = s.(Array.length s - 1) and last_h s = heading_at s (Array.length s - 1) in
  ( [ stalk_path s1; stalk_path s2 ]
    @ leaf pal ~at:(c +: v 0. 50.) ~heading:(up -. 1.15) ~length:56. ~bend:0.4 ()
    @ leaf pal ~at:(c +: v 0. 64.) ~heading:(up +. 1.1) ~length:50. ~bend:(-0.4) (),
    bud (last s1) (last_h s1) @ bud (last s2) (last_h s2)
    @ [ flower5 pal (Rng.key r "big") ~grad:"pa" ~at:c ~radius:70. ~tilt:(-12.) ~squash:0.86 ] )

(* The vine trails off at the bottom rather than stopping at the edge. *)
let fade =
  Printf.sprintf
    "<linearGradient id=\"fadeg\" x1=\"0\" y1=\"0\" x2=\"0\" y2=\"1\"><stop offset=\".88\" \
     stop-color=\"#fff\"/><stop offset=\"1\" stop-color=\"#000\"/></linearGradient><mask id=\"fade\" \
     maskUnits=\"userSpaceOnUse\" x=\"0\" y=\"0\" width=\"%d\" height=\"%d\"><rect width=\"%d\" \
     height=\"%d\" fill=\"url(#fadeg)\"/></mask>"
    (int_of_float width) (int_of_float height) (int_of_float width) (int_of_float height)

let svg ~crown pal =
  let r = Rng.make (match crown with Purkinje -> 1 | Tree -> 2 | Bloom -> 3) in
  let stem = main_stem r crown in
  let a = Array.of_list stem in
  let lengths = arc_lengths a in
  (* the first point above the leafy part *)
  let split =
    let rec go i = if i >= Array.length a - 1 || a.(i).y < plant_top crown then i else go (i + 1) in
    go 0
  in
  let plant = List.filteri (fun i _ -> i <= split) stem in
  let above = List.filteri (fun i _ -> i >= split) stem in
  let w0, w1, w2 =
    match crown with Purkinje -> (6.6, 2.4, 2.4) | Tree -> (8., 5.6, 7.) | Bloom -> (6.6, 3.2, 2.6)
  in
  let plant_stem = path (closed_d (ribbon plant (fun t -> lerp w0 w1 t))) [ fill pal.ink ] in
  let upper =
    match crown with
    | Purkinje -> axon pal above
    | Tree | Bloom -> [ path (closed_d (ribbon above (fun t -> lerp w1 w2 t))) [ fill pal.ink ] ]
  in
  (* a pale line down one side of the stem, to make it look round *)
  let highlight =
    let ns = normals a in
    List.init (max 0 (split - 14)) (fun k ->
        let i = k + 10 in
        a.(i) +: ((lerp w0 w1 (float i /. float split) /. 4.) *: ns.(i)))
    |> fun l -> path (line_d l) ([ no_fill; ("opacity", ".85") ] @ stroke pal.leaf_lit 0.7 @ round)
  in
  let rn = Rng.key r "nodes" in
  let positions =
    let top = lengths.(split) -. 40. in
    let rec go s i acc =
      if s > top then List.rev acc else go (s +. Rng.range (Rng.nth rn i) 46. 58.) (i + 1) (s :: acc)
    in
    go 50. 0 []
  in
  let nodes =
    List.mapi
      (fun i s ->
        let at = a.(index_at_length lengths s) in
        node pal (Rng.nth rn i) ~i ~at ~headroom:(at.y -. plant_top crown))
      positions
  in
  let vine = second_vine pal (Rng.key r "vine") in
  let crown_back, crown_front =
    match crown with Purkinje -> purkinje pal r | Tree -> tree pal r | Bloom -> bloom pal r
  in
  let body =
    vine.back @ crown_back
    @ List.concat_map (fun n -> n.back) nodes
    @ [ plant_stem; highlight ] @ upper @ vine.front
    @ List.concat_map (fun n -> n.front) nodes
    @ crown_front
  in
  Printf.sprintf
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\" width=\"%d\" \
     height=\"%d\"><defs>%s%s%s</defs><g mask=\"url(#fade)\"><g filter=\"url(#grain)\">%s</g></g></svg>"
    (int_of_float width) (int_of_float height) (int_of_float width) (int_of_float height)
    (gradients pal) fade
    (grain_filter ~w:(int_of_float width) ~h:(int_of_float height))
    (String.concat "" body)

let crowns = [ ("purkinje", Purkinje); ("tree", Tree); ("bloom", Bloom) ]
let palettes = [ forest; indigo ]

(** Every variant, as (file name, lazily drawn SVG). *)
let variants =
  List.concat_map
    (fun (cname, crown) ->
      List.map (fun pal -> (Printf.sprintf "%s-%s.svg" cname pal.name, lazy (svg ~crown pal))) palettes)
    crowns
