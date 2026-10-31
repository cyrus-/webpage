(* A Purkinje cell after Ramón y Cajal's drawings, grown roughly the way
   the real ones grow.

   From a dark soma and a few hand-placed main limbs, growth cones wander
   upward with some persistence, repelled by the cell's own dendrites
   (isoneuronal self-avoidance); they fork now and then where there is
   room, stall at the top of the molecular layer, which rises as the cell
   develops, and stop when they are hemmed in. Meanwhile new branches sprout
   from the sides of existing dendrites into whatever space is left. The
   result fills its territory with curving dendrites around small, rounded
   gaps.

   As in Flora, everything is a pure function of the seed: randomness is a
   hash of the key path, and the growth loop is a recursion over immutable
   maps. *)

open Flora

(* Cajal's ink, the generator's default. *)
let sepia = { forest with name = "sepia"; ink = "#2b1d12" }
let inks = [ sepia; forest; indigo ]

let ink_of_query q =
  match List.assoc_opt "ink" q with
  | Some name -> Option.value (List.find_opt (fun (pal : palette) -> pal.name = name) inks) ~default:sepia
  | None -> sepia

(** {1 Growth} *)

module Cells = Map.Make (struct
  type t = int * int

  let compare = compare
end)

module Ids = Map.Make (Int)

type node = { id : int; pos : v; parent : int; (* -1 for the root *) fine : bool (* grown by a branchlet *) }

let unit_v a =
  let l = Float.hypot a.x a.y in
  if l < 1e-9 then a else (1. /. l) *: a

let dist a b = Float.hypot (a.x -. b.x) (a.y -. b.y)
let clampf lo hi x = Float.max lo (Float.min hi x)

let cell_size = 8.
let cell p = (int_of_float (Float.floor (p.x /. cell_size)), int_of_float (Float.floor (p.y /. cell_size)))
let add_cell grid n = Cells.update (cell n.pos) (fun l -> Some (n :: Option.value l ~default:[])) grid

(* The nodes within [radius] of [p]. *)
let near grid p radius =
  let ci, cj = cell p in
  let k = int_of_float (Float.ceil (radius /. cell_size)) in
  List.init ((2 * k) + 1) (fun i -> i - k)
  |> List.concat_map (fun di -> List.init ((2 * k) + 1) (fun j -> (ci + di, cj + j - k)))
  |> List.concat_map (fun c -> Option.value (Cells.find_opt c grid) ~default:[])
  |> List.filter (fun n -> dist n.pos p < radius)

(* Two kinds of growth cone. Dendrite cones are long-lived: they swing in
   wide arcs and fork into Ys. Branchlet cones come later, sprouting from
   the sides of dendrites wherever there is room; they wiggle, fork often
   and soon stop, filling the gaps with a moss of fine spiny branchlets. *)
type kind = Dendrite | Branchlet

type style = {
  step : float; (* how far a cone moves per round *)
  feel : float; (* cones feel other dendrites this far away *)
  blocked_at : float; (* and stop when they come this close *)
  avoidance : float;
  turn_noise : float; (* turning rate: random kicks ... *)
  turn_memory : float; (* ... that persist, so cones swing in arcs *)
  max_turn : float;
  fork : float; (* chance per round, where there is room *)
  fork_angle : float * float;
  fork_skew : float * float; (* how much of the angle the first branch takes; small: a side branch *)
  stop : float; (* chance per round *)
}

let style = function
  | Dendrite ->
      {
        step = 2.;
        feel = 10.;
        blocked_at = 3.6;
        avoidance = 0.25;
        turn_noise = 0.1;
        turn_memory = 0.9;
        max_turn = 0.3;
        fork = 0.06;
        fork_angle = (0.9, 1.5);
        fork_skew = (0.08, 0.3);
        stop = 0.0008;
      }
  | Branchlet ->
      {
        step = 1.;
        feel = 3.5;
        blocked_at = 1.3;
        avoidance = 1.;
        turn_noise = 0.3;
        turn_memory = 0.55;
        max_turn = 0.45;
        fork = 0.;
        fork_angle = (0.5, 1.1);
        fork_skew = (0.25, 0.75);
        stop = 0.09;
      }

(* A growth cone: where it is, which way it is heading and turning, how many
   forks from the main limbs, and the last few nodes behind it. *)
type cone = { at : node; heading : float; turning : float; order : int; kind : kind; trail : int list }

type world = { grid : node list Cells.t; by_id : node Ids.t; next : int }

let add_node w n = { grid = add_cell w.grid n; by_id = Ids.add n.id n w.by_id; next = max w.next (n.id + 1) }

(* Other cells in the molecular layer (stellate and basket cell bodies),
   which the dendrites grow around. *)
type obstacle = { centre : v; radius : float }

(* One round for one cone: move it (or not), maybe fork it. Returns the
   world with any new node and the cones that carry on. *)
let advance r ~inside ~obstacles ~lift (w, cones) cone =
  let s = style cone.kind in
  let p = cone.at.pos in
  (* a cone ignores its own recent trail, and its siblings near where they forked *)
  let rec kin depth n =
    List.mem n.id cone.trail
    || (depth > 0 && match Ids.find_opt n.parent w.by_id with Some par -> kin (depth - 1) par | None -> false)
  in
  let others = near w.grid p s.feel |> List.filter (fun n -> not (kin 4 n)) in
  let repel q d = (((1. -. (d /. s.feel)) ** 2.) /. Float.max d 0.5) *: unit_v (p -: q) in
  let push =
    List.fold_left (fun acc n -> acc +: repel n.pos (dist p n.pos)) (v 0. 0.) others
    +: List.fold_left
         (fun acc o ->
           let d = dist p o.centre -. o.radius in
           if d < s.feel then acc +: repel o.centre (Float.max d 0.3) else acc)
         (v 0. 0.) obstacles
  in
  let turning =
    clampf (-.s.max_turn) s.max_turn
      ((s.turn_memory *. cone.turning) +. Rng.range (Rng.key r "turn") (-.s.turn_noise) s.turn_noise)
  in
  let lift = match cone.kind with Dendrite -> lift p | Branchlet -> 1.2 *. (lift p ** 1.6) in
  let want = unit_v (dir (cone.heading +. turning) +: (lift *: v 0. (-1.)) +: (s.avoidance *: push)) in
  let a = cone.heading +. clampf (-.s.max_turn) s.max_turn (Flora.turn_to cone.heading (angle_of want)) in
  let q = p +: (s.step *: dir a) in
  let walled = (not (inside q)) || List.exists (fun o -> dist q o.centre < o.radius) obstacles in
  let touching = near w.grid q s.blocked_at |> List.filter (fun n -> not (kin 4 n)) in
  if walled then (w, cones)
  else if touching <> [] then
    (* run into another dendrite: in a drawing of a flattened cell, branches
       that touch read as joined, which is what closes Cajal's loops *)
    match (cone.kind, touching) with
    | Dendrite, m :: _ when Rng.chance (Rng.key r "join") 0.85 ->
        (add_node w { id = w.next; pos = m.pos; parent = cone.at.id; fine = false }, cones)
    | _ -> (w, cones)
  else
    let n = { id = w.next; pos = q; parent = cone.at.id; fine = cone.kind = Branchlet } in
    let w = add_node w n in
    let trail = List.filteri (fun i _ -> i < 6) (n.id :: cone.trail) in
    let crowding = Float.min 1. (float (List.length others) /. 10.) in
    let stops = Rng.chance (Rng.key r "stop") (s.stop +. (0.0008 *. float cone.order)) in
    let forks = Rng.chance (Rng.key r "fork") (s.fork *. (1. -. crowding)) in
    if forks then
      (* a Y: both branches bend away from the old heading *)
      let lo, hi = s.fork_angle in
      let spread = Rng.range (Rng.key r "angle") lo hi in
      let skew_lo, skew_hi = s.fork_skew in
      let skew = Rng.range (Rng.key r "skew") skew_lo skew_hi in
      let skew = if Rng.chance (Rng.key r "mirror") 0.5 then 1. -. skew else skew in
      let branch h = { at = n; heading = h; turning = 0.; order = cone.order + 1; kind = cone.kind; trail } in
      (w, branch (a -. (spread *. skew)) :: branch (a +. (spread *. (1. -. skew))) :: cones)
    else if stops then (w, cones)
    else (w, { cone with at = n; heading = a; turning; trail } :: cones)

(* New cones from the sides of existing dendrites, into free space. *)
let sprouts r ~inside ~obstacles ~kind ~tries w =
  let probe_at, free = match kind with Dendrite -> (9., 7.5) | Branchlet -> (2.5, 1.8) in
  List.init tries (fun i ->
      let ri = Rng.nth r i in
      let id = int_of_float (Rng.range (Rng.key ri "id") 0. (float w.next)) in
      match Ids.find_opt id w.by_id with
      | None -> None
      | Some n -> (
          match Ids.find_opt n.parent w.by_id with
          | None -> None
          | Some par ->
              let along = angle_of (n.pos -: par.pos) in
              let side = if Rng.chance (Rng.key ri "side") 0.5 then 1. else -1. in
              let a = along +. (side *. Rng.range (Rng.key ri "a") 0.7 1.4) in
              let probe = n.pos +: (probe_at *: dir a) in
              if
                inside probe
                && (not (List.exists (fun o -> dist probe o.centre < o.radius +. 1.) obstacles))
                && near w.grid probe free = []
              then Some { at = n; heading = a; turning = 0.; order = 3; kind; trail = [ n.id; n.parent; par.parent ] }
              else None))
  |> List.filter_map Fun.id

(* Dendrites first, branchlets once they are mostly in place. *)
let grow r ~seed ~cones ~inside ~obstacles ~lift ~rounds ~branchlets_from =
  let w0 = List.fold_left add_node { grid = Cells.empty; by_id = Ids.empty; next = 0 } seed in
  let rec go t w cones =
    if t >= rounds then w
    else
      let rt = Rng.nth r t in
      let w, cones =
        List.fold_left
          (fun acc c -> advance (Rng.nth (Rng.nth rt c.at.id) c.order) ~inside ~obstacles ~lift acc c)
          (w, []) cones
      in
      let fresh =
        if t < branchlets_from then sprouts (Rng.key rt "dendrites") ~inside ~obstacles ~kind:Dendrite ~tries:40 w
        else sprouts (Rng.key rt "branchlets") ~inside ~obstacles ~kind:Branchlet ~tries:170 w
      in
      go (t + 1) w (fresh @ cones)
  in
  go 0 w0 cones

(** {1 Drawing} *)

let children nodes =
  List.fold_left
    (fun m n -> if n.parent < 0 then m else Ids.update n.parent (fun l -> Some (n :: Option.value l ~default:[])) m)
    Ids.empty nodes

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
   first child. *)
let smooth nodes =
  let by_id = List.fold_left (fun m n -> Ids.add n.id n m) Ids.empty nodes and kids = children nodes in
  List.map
    (fun n ->
      match (Ids.find_opt n.parent by_id, Ids.find_opt n.id kids) with
      | Some p, Some (c :: _) -> { n with pos = (0.5 *: n.pos) +: (0.25 *: p.pos) +: (0.25 *: c.pos) }
      | _ -> n)
    nodes

(* A closed Catmull-Rom curve through the points. *)
let closed_spline pts =
  let a = Array.of_list pts in
  let n = Array.length a in
  let p i = a.(((i mod n) + n) mod n) in
  cubic_d (p 0)
    (List.init n (fun i ->
         ( p i +: ((1. /. 6.) *: (p (i + 1) -: p (i - 1))),
           p (i + 1) -: ((1. /. 6.) *: (p (i + 2) -: p i)),
           p (i + 1) )))
  ^ "Z"

(* The cell body: a smooth oval, squashed flat, never quite the same twice. It
   leans a little, rises broadly into the trunk where the main limbs leave,
   and draws down into a hillock where the axon starts. Returns
   the outline and the tip of the hillock. *)
let soma_at r c ~limbs =
  let r = Rng.key r "soma" in
  let j k lo hi = Rng.range (Rng.key r k) lo hi in
  let radius = j "r" 8.5 10.5 and aspect = j "aspect" 0.58 0.78 and tilt = j "tilt" (-0.25) 0.25 in
  let wobble = List.map (fun k -> (k, j ("a" ^ string_of_int k) 0. 0.04, j ("p" ^ string_of_int k) 0. 6.3)) [ 2; 3 ] in
  let bump width d = exp (-.((Flora.turn_to 0. d /. width) ** 2.)) in
  (* one broad swelling toward the limbs, not a lump for each *)
  let trunk =
    let x, y = List.fold_left (fun (x, y) a -> (x +. cos a, y +. sin a)) (0., 0.) limbs in
    (Float.atan2 y x, j "t" 1.5 3., j "tw" 0.6 0.8)
  in
  let hillock = ((pi /. 2.) -. 0.25 +. j "ha" (-0.15) 0.15, j "h" 1.5 3., j "hw" 0.4 0.5) in
  (* a tilted ellipse, in polar form, so the bumps stay where they belong *)
  let ellipse t =
    let a = radius /. sqrt aspect and b = radius *. sqrt aspect in
    a *. b /. Float.hypot (b *. cos (t -. tilt)) (a *. sin (t -. tilt))
  in
  let rho t =
    ellipse t *. List.fold_left (fun acc (k, amp, ph) -> acc +. (amp *. cos ((float k *. t) +. ph))) 1. wobble
    +. List.fold_left (fun acc (a, amp, width) -> acc +. (amp *. bump width (t -. a))) 0. [ hillock; trunk ]
  in
  (* broad above, narrowing below: a teardrop *)
  let pear = j "pear" 0. 0.12 in
  let at t =
    let q = rho t *: dir t in
    c +: v (q.x *. (1. -. (pear *. q.y /. radius))) q.y
  in
  let ha, _, _ = hillock in
  (closed_spline (List.init 40 (fun i -> at (2. *. pi *. float i /. 40.))), at ha)

let w = 340.
let top = 14.

(** The cell, as an SVG document. *)
let draw ~seed:s pal =
  let r = Rng.make s in
  let soma = v (0.58 *. w) (top +. (0.95 *. w)) in
  let axon_length = 110. in
  let h = soma.y +. 16. +. axon_length in
  let root = { id = 0; pos = soma +: v 0. (-2.); parent = -1; fine = false } in
  (* the main limbs, placed by hand and lopsided as in Cajal's drawing: one
     long heavy limb sweeping out to the left with limbs of its own rising
     from it, and shorter, uneven ones to the right *)
  let chain parent first heading length =
    curve ~step:3. parent.pos length heading
    |> List.tl
    |> List.mapi (fun i q -> { id = first + i; pos = q; parent = (if i = 0 then parent.id else first + i - 1); fine = false })
  in
  let at l i = List.nth l (min (List.length l - 1) i) in
  let last l = List.nth l (List.length l - 1) in
  let rl = Rng.key r "limbs" in
  (* each seed bends and stretches the limbs a little differently *)
  let jig k lo hi = Rng.range (Rng.key rl k) lo hi in
  let wave ph s = 0.2 *. (sin ((s /. 14.) +. ph +. jig "ph" 0. 6.) +. (0.35 *. sin ((s /. 5.) +. (2. *. ph)))) in
  let left = chain root 1 (fun s -> up -. 1.42 +. jig "la" (-0.06) 0.06 +. (0.0016 *. s) +. wave 0.3 s) (190. *. jig "ll" 0.85 1.05) in
  let left_up = chain (at left (int_of_float (jig "lui" 16. 28.))) 100 (fun s -> up -. 0.35 +. jig "lua" (-0.2) 0.2 +. (0.004 *. s) +. wave 1.7 s) (58. *. jig "lul" 0.7 1.3) in
  let left_far = chain (at left (int_of_float (jig "lfi" 38. 52.))) 150 (fun s -> up -. 0.15 +. jig "lfa" (-0.2) 0.2 +. wave 2.2 s) (30. *. jig "lfl" 0.7 1.4) in
  let right = chain root 200 (fun s -> up +. 1.25 +. jig "ra" (-0.1) 0.1 -. (0.004 *. s) -. wave 0.9 s) (110. *. jig "rl" 0.8 1.15) in
  let middle = chain root 300 (fun s -> up +. 0.22 +. jig "ma" (-0.25) 0.25 -. (0.003 *. s) +. wave 2.9 s) (42. *. jig "ml" 0.7 1.4) in
  let middle_side = chain (at middle 9) 400 (fun s -> up -. 0.5 +. (0.008 *. s) +. wave 4.1 s) 30. in
  let limbs = [ left; left_up; left_far; right; middle; middle_side ] in
  let seed = root :: List.concat limbs in
  (* a cone at the end of each main limb, carrying on its way *)
  let cones =
    List.map
      (fun l ->
        let n = last l and m = at l (List.length l - 2) in
        { at = n; heading = angle_of (n.pos -: m.pos); turning = 0.; order = 1; kind = Dendrite; trail = [ n.id; m.id ] })
      limbs
    (* and, like a candelabrum, secondaries rising at intervals along the
       long sideways limbs *)
    @ List.concat_map
        (fun (key, l) ->
          List.filteri (fun i _ -> i > 4 && i < List.length l - 3) l
          |> List.filteri (fun i _ -> i mod 7 = 3)
          |> List.map (fun n ->
                 let rn = Rng.nth (Rng.key rl key) n.id in
                 { at = n; heading = up +. Rng.range rn (-0.35) 0.35; turning = 0.; order = 2; kind = Dendrite;
                   trail = [ n.id; n.parent ] }))
        [ ("lc", left); ("rc", right) ]
  in
  (* the territory: roughly square, but with ragged sides and top *)
  let ragged q k = 7. *. (sin ((q /. 23.) +. jig (k ^ "1") 0. 6.) +. (0.6 *. sin ((q /. 9.) +. jig (k ^ "2") 0. 6.))) in
  let inside q =
    q.x > 10. +. ragged q.y "l"
    && q.x < w -. 10. -. ragged q.y "r"
    && q.y > top +. 5. +. ragged q.x "t"
    && q.y < soma.y -. 14. -. (0.07 *. Float.abs (q.x -. soma.x))
  in
  let ro = Rng.key r "obstacles" in
  let obstacles =
    List.init 70 (fun i ->
        let ri = Rng.nth ro i in
        { centre = v (Rng.range (Rng.key ri "x") 10. (w -. 10.)) (Rng.range (Rng.key ri "y") (top +. 10.) (soma.y -. 50.));
          radius = Rng.range (Rng.key ri "r") 4. 10. })
    |> List.filter (fun o -> List.for_all (fun l -> List.for_all (fun n -> dist n.pos o.centre > o.radius +. 4.) l) limbs)
  in
  (* growth drifts upward, more so higher up, and turns straight up just
     below the surface of the layer *)
  let lift q =
    let rise = clampf 0. 1. ((soma.y -. q.y) /. (soma.y -. top)) in
    0.1 +. (0.2 *. rise) +. (0.9 *. (Float.max 0. ((rise -. 0.86) /. 0.14) ** 2.))
  in
  let world = grow (Rng.key r "grow") ~seed ~cones ~inside ~obstacles ~lift ~rounds:520 ~branchlets_from:360 in
  let nodes = Ids.fold (fun _ n acc -> n :: acc) world.by_id [] |> smooth |> smooth in
  (* widths follow the dendrites alone, not the fine strands on them *)
  let tips = tips (List.filter (fun n -> not n.fine) nodes) and kids = children nodes in
  let tips_of n = Option.value (Ids.find_opt n.id tips) ~default:1 in
  let kids_of n = Option.value (Ids.find_opt n.id kids) ~default:[] in
  (* how far each node's dendrite (not branchlet) subtree reaches, in nodes *)
  let reach =
    List.sort (fun a b -> compare b.id a.id) nodes
    |> List.fold_left
         (fun m n ->
           let d = if n.fine then 0 else 1 + Option.value (Ids.find_opt n.id m) ~default:0 in
           let m = Ids.add n.id d m in
           if n.parent < 0 then m
           else Ids.update n.parent (fun s -> Some (max d (Option.value s ~default:0))) m)
         Ids.empty
  in
  let reach_of n = Option.value (Ids.find_opt n.id reach) ~default:0 in
  (* pen width in tenths. Cajal's dendrites keep their weight: the trunks
     are only somewhat heavier than the branches they feed (a flat pipe
     model), and each dendrite stays medium weight until near its end,
     where it tapers over its last few dozen steps to a rounded tip *)
  let pen n =
    if n.fine then 5.
    else
      let base = clampf 1.9 4.4 (1.35 *. (float (tips_of n) ** 0.27)) in
      let taper = Float.min 1. (float (reach_of n) /. 22.) ** 0.55 in
      2. *. Float.round (5. *. (1.1 +. ((base -. 1.1) *. taper)))
  in
  (* unbranched runs, each one polyline of one width *)
  let rec runs from n =
    let rec follow n acc = match kids_of n with [ c ] when pen c = pen n -> follow c (c.pos :: acc) | _ -> (n, List.rev acc) in
    let last, pts = follow n [ n.pos ] in
    (pen n, line_d (from :: pts)) :: List.concat_map (runs last.pos) (kids_of last)
  in
  let segments = List.concat_map (runs root.pos) (kids_of root) in
  (* ink pooling in the crotch where a thick dendrite forks *)
  let blots =
    List.filter_map
      (fun n ->
        match kids_of n with
        | _ :: _ :: _ when pen n >= 14. -> Some (circle n.pos (pen n /. 10. *. 0.6) [ fill pal.ink ])
        | _ -> None)
      nodes
  in
  let dendrites =
    List.sort_uniq compare (List.map fst segments)
    |> List.map (fun pw ->
           let d = List.filter_map (fun (pw', s) -> if pw' = pw then Some s else None) segments in
           path (String.concat "" d) ([ no_fill ] @ stroke pal.ink (pw /. 10.) @ round))
  in
  let outline, hillock = soma_at r soma ~limbs:(List.map (fun l -> angle_of ((at l 4).pos -: soma)) [ left; right; middle ]) in
  (* the axon, falling away below with one recurrent collateral *)
  let axon =
    let pts = curve ~step:2. hillock axon_length (fun s -> (pi /. 2.) -. 0.4 +. (0.1 *. sin (s /. 13.))) in
    let a = Array.of_list pts in
    let j = Array.length a / 2 in
    let collateral = curve ~step:1.5 a.(j) (axon_length *. 0.9) (fun s -> (pi /. 2.) +. 1.05 -. (s *. 0.004) +. (0.12 *. sin (s /. 9.))) in
    [
      path (line_d pts) ([ no_fill ] @ stroke pal.ink 1.1 @ round);
      path (line_d collateral) ([ no_fill ] @ stroke pal.ink 0.6 @ round);
    ]
  in
  let body = dendrites @ blots @ [ path outline ([ fill pal.ink ] @ stroke pal.ink 0.6) ] @ axon in
  let wi = int_of_float (Float.ceil w) and hi = int_of_float (Float.ceil h) in
  Printf.sprintf
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\" width=\"%d\" \
     height=\"%d\">%s</svg>"
    wi hi wi hi (String.concat "" body)
