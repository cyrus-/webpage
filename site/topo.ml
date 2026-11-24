(* Topo: the map lines behind the page, as a seamless tile of vector
   contours, so they stay crisp at any zoom (they replace a blurred raster
   texture, images/cartographer.png).

   The terrain is a sum of waves whose frequencies are whole numbers of
   cycles across the tile, so it wraps around exactly: contours that leave
   one edge come back in at the opposite one. They are traced by marching
   squares and drawn as one stroked path; the stylesheet uses the tile as a
   mask, so it decides the colour.

   Pure: "random" choices are hashes of fixed keys, so every build draws the
   same map. *)

let tile = 960. (* CSS pixels per tile *)
let n = 480 (* grid cells per side *)
let seed = 11

let unit k = float_of_int (Hashtbl.hash ("topo", seed, k) land 0xffffff) /. 16777216.
let range k lo hi = lo +. ((hi -. lo) *. unit k)

(* The waves: (kx, ky, amplitude, phase), with frequencies up to [kmax]
   cycles per tile, and amplitudes falling off with frequency so the
   terrain has broad hills with smaller ones on their slopes. *)
let waves =
  let kmax = 9 in
  List.concat_map
    (fun kx ->
      List.filter_map
        (fun ky ->
          let k = sqrt (float ((kx * kx) + (ky * ky))) in
          (* each wave once: skip (0,0) and the mirror image of a half-plane *)
          if k = 0. || k > float kmax || ky < 0 || (ky = 0 && kx < 0) then None
          else
            let key = (kx, ky) in
            let amp = range (key, "a") 0.4 1. /. (k ** 1.6) in
            Some (float kx, float ky, amp, range (key, "p") 0. (2. *. Float.pi)))
        (List.init ((2 * kmax) + 1) (fun i -> i - kmax)))
    (List.init ((2 * kmax) + 1) (fun i -> i - kmax))

(* Heights on the (n+1) x (n+1) grid; row and column n repeat 0. *)
let heights =
  lazy
    (let w = 2. *. Float.pi /. float n in
     let h =
       Array.init (n + 1) (fun j ->
           Array.init (n + 1) (fun i ->
               List.fold_left
                 (fun s (kx, ky, a, p) -> s +. (a *. cos ((w *. ((kx *. float i) +. (ky *. float j))) +. p)))
                 0. waves))
     in
     (* scale to unit standard deviation *)
     let all = Array.concat (Array.to_list h) in
     let m = Array.fold_left ( +. ) 0. all /. float (Array.length all) in
     let var = Array.fold_left (fun s x -> s +. ((x -. m) ** 2.)) 0. all /. float (Array.length all) in
     Array.map (Array.map (fun x -> (x -. m) /. sqrt var)) h)

(* {1 Marching squares}

   A crossing point is named by the grid edge it lies on: [`H (i, j)] on the
   edge from (i, j) to (i+1, j), [`V (i, j)] on the one from (i, j) to
   (i, j+1). Each cell contributes segments between the crossings on its
   sides; chaining segments that share a crossing gives the contour lines. *)

type edge = H of int * int | V of int * int

let segments h level =
  let segs = ref [] in
  for j = 0 to n - 1 do
    for i = 0 to n - 1 do
      let a = h.(j).(i) and b = h.(j).(i + 1) and c = h.(j + 1).(i + 1) and d = h.(j + 1).(i) in
      let bit x k = if x > level then k else 0 in
      let top = H (i, j) and right = V (i + 1, j) and bottom = H (i, j + 1) and left = V (i, j) in
      let add e f = segs := (e, f) :: !segs in
      match bit a 1 lor bit b 2 lor bit c 4 lor bit d 8 with
      | 0 | 15 -> ()
      | 1 | 14 -> add left top
      | 2 | 13 -> add top right
      | 3 | 12 -> add left right
      | 4 | 11 -> add right bottom
      | 6 | 9 -> add top bottom
      | 7 | 8 -> add left bottom
      | 5 | 10 as k ->
          (* a saddle: decide by the height in the middle of the cell *)
          let mid_high = (a +. b +. c +. d) /. 4. > level in
          if (k = 5) = mid_high then (add left bottom; add top right) else (add left top; add right bottom)
      | _ -> ()
    done
  done;
  !segs

let point h level = function
  | H (i, j) ->
      let a = h.(j).(i) and b = h.(j).(i + 1) in
      (float i +. ((level -. a) /. (b -. a)), float j)
  | V (i, j) ->
      let a = h.(j).(i) and b = h.(j + 1).(i) in
      (float i, float j +. ((level -. a) /. (b -. a)))

(* Chain segments into polylines (lists of crossings). *)
let chains segs =
  let adj = Hashtbl.create 4096 in
  List.iter
    (fun (e, f) ->
      Hashtbl.add adj e f;
      Hashtbl.add adj f e)
    segs;
  let used = Hashtbl.create 4096 in
  let key e f = if compare e f < 0 then (e, f) else (f, e) in
  let next prev cur =
    List.find_opt (fun f -> f <> prev && not (Hashtbl.mem used (key cur f))) (Hashtbl.find_all adj cur)
  in
  let rec walk prev cur acc =
    match next prev cur with
    | Some f ->
        Hashtbl.replace used (key cur f) ();
        walk cur f (f :: acc)
    | None -> acc
  in
  List.filter_map
    (fun (e, f) ->
      if Hashtbl.mem used (key e f) then None
      else begin
        Hashtbl.replace used (key e f) ();
        (* extend both ways from this segment *)
        let forward = walk e f [ f; e ] in
        let back = walk f e [] in
        (* [back] runs from its far end in to [e]; [forward] from its far end back to [e] *)
        Some (back @ List.rev forward)
      end)
    segs

(* Drop points that lie within [tol] of the line through their neighbours. *)
let rec simplify tol = function
  | [] | [ _ ] | [ _; _ ] as l -> l
  | pts ->
      let a = Array.of_list pts in
      let k = Array.length a in
      let (x0, y0) = a.(0) and (x1, y1) = a.(k - 1) in
      let dx = x1 -. x0 and dy = y1 -. y0 in
      let len = sqrt ((dx *. dx) +. (dy *. dy)) in
      let dist (x, y) =
        if len = 0. then sqrt (((x -. x0) ** 2.) +. ((y -. y0) ** 2.))
        else Float.abs ((dy *. (x -. x0)) -. (dx *. (y -. y0))) /. len
      in
      let best = ref 0 and dmax = ref 0. in
      for i = 1 to k - 2 do
        let d = dist a.(i) in
        if d > !dmax then (dmax := d; best := i)
      done;
      if !dmax <= tol then [ a.(0); a.(k - 1) ]
      else
        let left = simplify tol (Array.to_list (Array.sub a 0 (!best + 1))) in
        let right = simplify tol (Array.to_list (Array.sub a !best (k - !best))) in
        left @ List.tl right

let num x =
  let s = Printf.sprintf "%.1f" x in
  if String.ends_with ~suffix:".0" s then String.sub s 0 (String.length s - 2) else s

(* Contour levels, one every [spacing] standard deviations. *)
let spacing = 0.34

let svg =
  lazy
    (let h = Lazy.force heights in
     let scale = tile /. float n in
     let levels = List.init 31 (fun k -> spacing *. (float (k - 15) +. 0.5)) in
     let d = Buffer.create 65536 in
     List.iter
       (fun level ->
         List.iter
           (fun chain ->
             let pts = List.map (fun e -> let x, y = point h level e in (x *. scale, y *. scale)) chain in
             match simplify 0.12 pts with
             | [] -> ()
             | (x, y) :: rest ->
                 Buffer.add_string d (Printf.sprintf "M%s %s" (num x) (num y));
                 List.iter (fun (x, y) -> Buffer.add_string d (Printf.sprintf "L%s %s" (num x) (num y))) rest)
           (chains (segments h level)))
       levels;
     let t = num tile in
     Printf.sprintf
       "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %s %s\" width=\"%s\" height=\"%s\"><path \
        d=\"%s\" fill=\"none\" stroke=\"#000\" stroke-width=\"1\" stroke-linejoin=\"round\"/></svg>"
       t t t t (Buffer.contents d))
