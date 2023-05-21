type v2 = Contour.v2

type dir = Horz | Vert

type 'a t = dir -> 'a Layout.t

let make (width, height) fn _ = Layout.make ~width ~height fn

let extract t =
  let t = Layout.realign (t Horz) in
  Layout.size t, Layout.get t

let map fn t dir = Layout.map fn (t dir)

let map2 fn a b dir =
  match dir with
  | Horz -> Layout.horz fn (a dir) (b dir)
  | Vert -> Layout.vert fn (a dir) (b dir)

let ( <$> ) = map

let ( <*> ) a b = map2 (fun f x -> f x) a b

let horz fn _ = fn Horz

let vert fn _ = fn Vert

module Syntax = struct
  let ( let+ ) x f = f <$> x

  let ( and+ ) a b = map2 (fun a b -> a, b) a b
end

let box t dir =
  let t, contour = t dir in
  let x0, _ = Contour.size_x contour in
  let t (x, y) = t (x -. x0, y) in
  t, Contour.box contour

let size t dir =
  let _, contour = t dir in
  let w, h = Contour.size contour in
  let min_x, _ = Contour.size_x contour in
  (fun (x, y) -> (x +. min_x, y), (w, h)), Contour.empty

let contour t dir =
  let _, contour = t dir in
  (fun (x, y) -> Contour.export (x, y) contour), Contour.empty

let padding size = function
  | Horz -> (fun _ -> ()), Contour.hpadding size
  | Vert -> (fun _ -> ()), Contour.vpadding size

let pad = padding

let pair ?padding a b =
  match padding with
  | None -> Syntax.( and+ ) a b
  | Some p ->
      let open Syntax in
      let+ a and+ () = pad p and+ b in
      a, b

let rec list ~p = function
  | [] -> make (0.0, 0.0) (fun _ -> [])
  | [x] -> map (fun x -> [x]) x
  | x :: xs ->
      let open Syntax in
      let+ xs = list ~p xs and+ () = padding p and+ x in
      x :: xs

let list ?(padding = 0.0) ts = map List.rev (list ~p:padding (List.rev ts))
