module Average = struct
  type t = float * int

  let zero = 0.0, 0

  let of_float x = x, 1

  let to_float (x, n) = x /. float n

  let add (x, n) (y, m) = x +. y, n + m

  let offset v (x, n) = x +. (v *. float n), n
end

type v2 = float * float

type slice =
  | Offset of float
  | Rect of {min_x: float; max_x: float; height: float}

type t =
  { center: Average.t
  ; bot_center: Average.t
  ; height: float
  ; shape: slice list
  ; left_padding: float
  ; right_padding: float
  ; bottom_offset: float }

let empty =
  { center= Average.zero
  ; bot_center= Average.zero
  ; height= 0.0
  ; shape= []
  ; left_padding= 0.0
  ; right_padding= 0.0
  ; bottom_offset= 0.0 }

let make ~width ~height =
  let center = Average.of_float (width /. 2.0) in
  { center
  ; bot_center= center
  ; height
  ; shape= [Rect {min_x= 0.0; max_x= width; height}]
  ; left_padding= 0.0
  ; right_padding= 0.0
  ; bottom_offset= 0.0 }

let vpadding height = make ~width:0.0 ~height

let hpadding width =
  { center= Average.zero
  ; bot_center= Average.zero
  ; height= 0.0
  ; shape= []
  ; left_padding= width
  ; right_padding= 0.0
  ; bottom_offset= 0.0 }

let rec size_x ~offset = function
  | [] -> 0.0, 0.0, offset
  | Offset dx :: rest -> size_x ~offset:(offset +. dx) rest
  | Rect {min_x; max_x; _} :: rest ->
      List.fold_left
        (fun (min_x, max_x, offset) -> function
          | Offset dx -> min_x, max_x, offset +. dx
          | Rect {min_x= x0; max_x= x1; _} ->
              min min_x (offset +. x0), max max_x (offset +. x1), offset )
        (min_x +. offset, max_x +. offset, offset)
        rest

let size_x t =
  let min_x, max_x, _ = size_x ~offset:0.0 t.shape in
  min_x, max_x

let width t =
  let min_x, max_x = size_x t in
  max_x -. min_x

let height t = t.height

let size (t : t) = width t, t.height

let center t = Average.to_float t.center

let bottom_center t = Average.to_float t.bot_center

let get_bottom_offset t = t.bottom_offset

let box t =
  let w = width t in
  let center = Average.of_float (w /. 2.0) in
  { center
  ; bot_center= center
  ; height= t.height
  ; shape= [Rect {min_x= 0.0; max_x= w; height= t.height}]
  ; left_padding= t.left_padding
  ; right_padding= t.right_padding
  ; bottom_offset= 0.0 }

let cons_offset dx = function
  | [] -> []
  | Offset dx' :: shape -> Offset (dx +. dx') :: shape
  | shape when dx = 0.0 -> shape
  | shape -> Offset dx :: shape

let append a b =
  let a_offset = get_bottom_offset a in
  { center= a.center
  ; bot_center= b.bot_center
  ; height= a.height +. b.height
  ; shape= a.shape @ cons_offset (-.a_offset) b.shape (* todo *)
  ; left_padding= 0.0
  ; right_padding= 0.0
  ; bottom_offset= b.bottom_offset }

let max_opt opt v =
  match opt with
  | None -> Some v
  | Some r -> Some (max r v)

let rec offset ~ox ~oy acc hx xs hy ys =
  match xs, ys with
  | Offset dx :: xs, _ -> offset ~ox:(ox +. dx) ~oy acc hx xs hy ys
  | _, Offset dy :: ys -> offset ~ox ~oy:(oy +. dy) acc hx xs hy ys
  | ( Rect {max_x= x1; height= h_left; _} :: xs'
    , Rect {min_x= y0; height= h_right; _} :: ys' ) ->
      let acc' = x1 +. ox -. (y0 +. oy) in
      let acc = max_opt acc acc' in
      let hx' = hx +. h_left in
      let hy' = hy +. h_right in
      if hx' = hy'
      then offset ~ox ~oy acc hx' xs' hy' ys'
      else if hx' < hy'
      then offset ~ox ~oy acc hx' xs' hy ys
      else offset ~ox ~oy acc hx xs hy' ys'
  | [], _ -> acc
  | _, [] -> acc

let offset xs ys =
  match offset ~ox:0.0 ~oy:0.0 None 0.0 xs 0.0 ys with
  | None -> 0.0
  | Some r -> r

let offset a b =
  let pad = a.right_padding +. b.left_padding in
  pad +. offset a.shape b.shape

let add offset t =
  { center= Average.offset offset t.center
  ; bot_center= Average.offset offset t.bot_center
  ; height= t.height
  ; shape= cons_offset offset t.shape
  ; left_padding= t.left_padding
  ; right_padding= t.right_padding
  ; bottom_offset= t.bottom_offset +. offset }

type side = Neither | Left | Right

let rec merge ~ox ~oy h hx xs hy ys =
  match xs, ys with
  | [], [] -> Neither, []
  | Offset dx :: xs, _ -> merge ~ox:(ox +. dx) ~oy h hx xs hy ys
  | _, Offset dy :: ys -> merge ~ox ~oy:(oy +. dy) h hx xs hy ys
  | ( Rect {min_x= x0; height= h_left; _} :: xs'
    , Rect {max_x= y1; height= h_right; _} :: ys' ) ->
      let hx' = hx +. h_left in
      let hy' = hy +. h_right in
      let h_min = min hx' hy' in
      let line = Rect {min_x= x0 +. ox; max_x= y1 +. oy; height= h_min -. h} in
      let side, rest =
        if hx' = hy'
        then merge ~ox ~oy h_min hx' xs' hy' ys'
        else if hx' < hy'
        then merge ~ox ~oy h_min hx' xs' hy ys
        else merge ~ox ~oy h_min hx xs hy' ys'
      in
      side, line :: rest
  | Rect {min_x; max_x; height} :: xs, [] ->
      let hx' = hx +. height in
      let h_rest = hx' -. hy in
      Left, Offset ox :: Rect {min_x; max_x; height= h_rest} :: xs
  | [], Rect {min_x; max_x; height} :: ys ->
      let hy' = hy +. height in
      let h_rest = hy' -. hx in
      Right, Offset oy :: Rect {min_x; max_x; height= h_rest} :: ys

let merge a b =
  let left_padding =
    match a.shape with
    | [] -> a.left_padding +. a.right_padding +. b.left_padding
    | _ -> a.left_padding
  in
  let right_padding =
    match b.shape with
    | [] -> a.right_padding +. b.left_padding +. b.right_padding
    | _ -> b.right_padding
  in
  let side, shape = merge ~ox:0.0 ~oy:0.0 0.0 0.0 a.shape 0.0 b.shape in
  let bottom_offset =
    match side with
    | Neither -> 0.0
    | Left -> a.bottom_offset
    | Right -> b.bottom_offset
  in
  { center= Average.add a.center b.center
  ; bot_center= Average.add a.bot_center b.bot_center
  ; height= max a.height b.height
  ; shape
  ; left_padding
  ; right_padding
  ; bottom_offset }

let export (x, y) t =
  let dx, dy = ref 0.0, ref 0.0 in
  List.filter_map
    (function
      | Offset offset ->
          dx := !dx +. offset ;
          None
      | Rect line ->
          let x = line.min_x +. !dx +. x in
          let w = line.max_x -. line.min_x in
          let y = y +. !dy in
          dy := !dy +. line.height ;
          let box = (x, y), (w, line.height) in
          Some box )
    t.shape
