module Image : sig
  type t

  val make : int * int -> t

  val set : t -> int * int -> string -> unit

  val draw_string : t -> int * int -> string -> unit

  val draw_box : t -> int * int -> int * int -> unit

  val draw_hline : t -> int -> int -> int -> unit

  val draw : t -> unit
end = struct
  type t = string array array

  let make (width, height) = Array.make_matrix height width " "

  let set t (x, y) chr = try t.(y).(x) <- chr with Invalid_argument _ -> ()

  let draw_string t (x, y) str =
    for i = 0 to String.length str - 1 do
      set t (x + i, y) (String.make 1 str.[i])
    done

  let draw_box t (x, y) (w, h) =
    for i = 1 to w - 1 do
      set t (x + i, y) "━" ;
      set t (x + i, y + h) "━"
    done ;
    for i = 1 to h - 1 do
      set t (x, y + i) "┃" ;
      set t (x + w, y + i) "┃"
    done ;
    set t (x, y) "┏" ;
    set t (x + w, y) "┓" ;
    set t (x + w, y + h) "┛" ;
    set t (x, y + h) "┗"

  let draw_hline img min_x max_x y =
    for x = min_x to max_x do
      set img (x, y) "─"
    done

  let draw t =
    Array.iter
      (fun line ->
        let line = String.concat "" (Array.to_list line) in
        print_endline line )
      t
end

module T = Prettree

type t = Leaf of string | Rose of string * t list

let int x = int_of_float x

let node ~box str =
  let len = String.length str in
  let width = float (4 + len) in
  T.make (width, 3.0) (fun (x, y) img ->
      let half = width /. 2.0 in
      let x, y = int x, int y in
      let half = int half in
      Image.draw_string img (x + 2, y + 1) str ;
      if box then Image.draw_box img (x, y) (int width - 1, 2) ;
      x + half, y )

let rec render = function
  | Leaf str ->
      T.make
        (float (String.length str), 2.0)
        (fun (x, y) img ->
          let x, y = int x, int y in
          Image.draw_string img (x, y + 1) str ;
          x + (String.length str / 2), y )
  | Rose (root, children) ->
      T.vert
        (let open T.Syntax in
        let+ root = node ~box:true root
        and+ () = T.padding 1.0
        and+ children =
          T.horz @@ T.list ~padding:3.0 @@ List.map render children
        in
        fun img ->
          let ((rx, ry) as root_pos) = root img in
          let children_pos = List.map (fun child -> child img) children in
          let nb_children = List.length children_pos in
          let min_x, _ = List.hd children_pos in
          let max_x, _ = List.hd (List.rev children_pos) in
          Image.draw_hline img min_x max_x (ry + 3) ;
          List.iteri
            (fun i (cx, cy) ->
              Image.set img (cx, cy) "┷" ;
              Image.set img
                (cx, cy - 1)
                ( if nb_children = 1
                then "│"
                else if i = 0
                then "┌"
                else if i + 1 = nb_children
                then "┐"
                else "┬" ) )
            children_pos ;
          Image.set img (rx, ry + 2) "┯" ;
          if nb_children > 1 then Image.set img (rx, ry + 3) "┴" ;
          root_pos)

let rec perfect n =
  if n = 0
  then Leaf (string_of_int n)
  else
    let child = perfect (n - 1) in
    Rose (string_of_int n, [child; child])

let rec fib n =
  if n <= 1
  then n, Leaf (string_of_int n)
  else
    let a, ta = fib (n - 2) in
    let b, tb = fib (n - 1) in
    let sum = a + b in
    sum, Rose (string_of_int sum, [ta; tb])

let _, test = fib 7

let test = Rose ("fibonacci", [test])

let int x = int_of_float (ceil x)

let () = print_endline ""

let () =
  let test = render test in
  let (w, h), fn = T.extract test in
  let img = Image.make (int w, int h) in
  let _ = fn (0.0, 0.0) img in
  Image.draw img ;
  let test = Rose ("binary trees", [perfect 3; perfect 1; perfect 4]) in
  let test = render test in
  let (w, h), fn = T.extract test in
  let img = Image.make (int w, int h) in
  let _ = fn (0.0, 0.0) img in
  Image.draw img
