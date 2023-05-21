type v2 = Contour.v2

type 'a t = {horz: 'a Layout.t Lazy.t; vert: 'a Layout.t Lazy.t}

let make (width, height) fn =
  let layout = lazy (Layout.make ~width ~height fn) in
  {horz= layout; vert= layout}

let get_horz t = Lazy.force t.horz

let get_vert t = Lazy.force t.vert

let extract t =
  let t = Layout.realign (get_horz t) in
  Layout.size t, Layout.get t

let map fn t =
  { horz= lazy (Layout.map fn (get_horz t))
  ; vert= lazy (Layout.map fn (get_vert t)) }

let map2 fn a b =
  { horz= lazy (Layout.horz fn (get_horz a) (get_horz b))
  ; vert= lazy (Layout.vert fn (get_vert a) (get_vert b)) }

let ( <$> ) = map

let ( <*> ) a b = map2 (fun f x -> f x) a b

let horz t = {t with vert= t.horz}

let vert t = {t with horz= t.vert}

module Syntax = struct
  let ( let+ ) x f = f <$> x

  let ( and+ ) a b = map2 (fun a b -> a, b) a b
end

let box t =
  let bb, result = extract t in
  make bb result

let get_size (_, contour) =
  let w, h = Contour.size contour in
  let min_x, _ = Contour.size_x contour in
  (fun (x, y) -> (x +. min_x, y), (w, h)), Contour.empty

let size t =
  {horz= lazy (get_size (get_horz t)); vert= lazy (get_size (get_vert t))}

let get_contour (_, contour) =
  (fun (x, y) -> Contour.export (x, y) contour), Contour.empty

let contour t =
  {horz= lazy (get_contour (get_horz t)); vert= lazy (get_contour (get_vert t))}

let padding size =
  { horz= lazy ((fun _ -> ()), Contour.hpadding size)
  ; vert= lazy ((fun _ -> ()), Contour.vpadding size) }

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
