type v2 = Contour.v2

type 'a t = (v2 -> 'a) * Contour.t

let size (_, c) = Contour.size c

let get (x, _) = x

let make ~width ~height x = x, Contour.make ~width ~height

let realign (t, contour) =
  let x0, _ = Contour.size_x contour in
  let t (x, y) = t (x -. x0, y) in
  t, contour

let map fn (g, contour) = (fun x -> fn (g x)), contour

let horz fn (a, a_contour) (b, b_contour) =
  let offset = Contour.offset a_contour b_contour in
  let b_contour = Contour.add offset b_contour in
  let contour = Contour.merge a_contour b_contour in
  let result (x, y) = fn (a (x, y)) (b (x +. offset, y)) in
  result, contour

let vert fn (a, a_contour) (b, b_contour) =
  let a_center = Contour.bottom_center a_contour in
  let b_center = Contour.center b_contour in
  let offset = a_center -. b_center in
  let contour = Contour.append a_contour (Contour.add offset b_contour) in
  let a_height = Contour.height a_contour in
  let result (x, y) =
    let y' = y +. a_height in
    fn (a (x, y)) (b (x +. offset, y'))
  in
  result, contour
