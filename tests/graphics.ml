module T = Prettree
module G = Graphics

let int x = int_of_float (floor (25.0 *. x))

let draw_circle (x, y) radius = G.draw_circle (int x) (int y) radius

let fill_circle (x, y) radius = G.fill_circle (int x) (int y) radius

let fill_rect (x, y) (w, h) = G.fill_rect (int x) (int y) (int w) (int h)

let draw_rect (x, y) (w, h) = G.draw_rect (int x) (int y) (int w) (int h)

let draw_line (x0, y0) (x1, y1) =
  G.moveto (int x0) (int y0) ;
  G.lineto (int x1) (int y1)

type t = Leaf | Single of t | Bin of t * t

let () = Random.self_init ()

let rec layout ~depth =
  let depth = depth + 1 in
  function
  | Leaf ->
      let diameter = 0.5 +. Random.float 1.5 in
      T.make (diameter, diameter) (fun (x, y) ->
          let radius = diameter /. 2.0 in
          let x = x +. radius in
          let y = y +. radius in
          G.set_color (G.rgb 80 80 80) ;
          fill_circle (x, y) (int radius) ;
          G.set_color G.black ;
          draw_circle (x, y) (int radius) ;
          x, y -. radius )
  | Bin (left, right) ->
      let height = 0.5 +. Random.float 2.0 in
      let width = 1.0 +. Random.float 2.0 in
      T.vert
        (let open T.Syntax in
         let+ parent, parent_bot =
           T.make (width, height) (fun (x, y) ->
               G.set_color (G.rgb 200 200 200) ;
               fill_rect (x, y) (width, height) ;
               G.set_color G.black ;
               draw_rect (x, y) (width, height) ;
               let x = x +. (width /. 2.0) in
               (x, y), (x, y +. height) )
         and+ () = T.padding 1.0
         and+ left, right =
           T.horz
           @@ let+ left = layout ~depth left
              and+ () = T.padding 0.5
              and+ right = layout ~depth right in
              left, right
         in
         draw_line parent_bot left ; draw_line parent_bot right ; parent )
  | Single child ->
      let height = 1.0 +. Random.float 2.0 in
      let width = 1.0 +. Random.float 2.0 in
      T.vert
        (let open T.Syntax in
         let+ parent, parent_bot =
           T.make (width, height) (fun (x, y) ->
               G.set_color (G.rgb 200 200 200) ;
               fill_rect (x, y) (width, height) ;
               G.set_color G.black ;
               draw_rect (x, y) (width, height) ;
               let x = x +. (width /. 2.0) in
               (x, y), (x, y +. height) )
         and+ () = T.padding 0.5
         and+ child = layout ~depth child in
         draw_line parent_bot child ; parent )

let rec layout2 ~depth =
  let depth = depth + 1 in
  function
  | Leaf ->
      T.make (1.0, 1.0) (fun (x, y) ->
          let x, y = x +. 0.5, y +. 0.5 in
          draw_circle (x, y) 6 ;
          x, y )
  | Single child ->
      T.vert
        (let open T.Syntax in
         let+ child = layout2 ~depth child
         and+ () = T.padding 0.5
         and+ parent =
           T.make (1.0, 1.0) (fun (x, y) ->
               let x, y = x +. 0.5, y +. 0.5 in
               fill_circle (x, y) 10 ;
               x, y )
         in
         draw_line parent child ; parent )
  | Bin (left, right) ->
      let tree =
        T.vert
          (let open T.Syntax in
           let+ left, right =
             T.horz
               (T.pair ~padding:0.5 (layout2 ~depth left) (layout2 ~depth right))
           and+ () = T.padding 0.5
           and+ parent =
             T.make (1.0, 1.0) (fun (x, y) ->
                 let x, y = x +. 0.5, y +. 0.5 in
                 fill_circle (x, y) 10 ;
                 x, y )
           in
           draw_line parent left ; draw_line parent right ; parent )
      in
      if depth <> -1
      then tree
      else
        let open T.Syntax in
        let+ ccc = T.contour tree and+ r = tree in
        G.set_color G.blue ;
        List.iter (fun ((x, y), (w, h)) -> draw_rect (x, y) (w, h)) ccc ;
        G.set_color G.black ;
        r

let layout2 t = layout2 ~depth:0 t

let rec layout3 ~depth =
  let depth = depth + 1 in
  function
  | Leaf ->
      T.make (1.0, 1.0) (fun (x, y) ->
          let x, y = x +. 0.5, y +. 0.5 in
          draw_circle (x, y) 6 ;
          x, y )
  | Single child ->
      T.vert
        (let open T.Syntax in
         let+ parent =
           T.make (1.0, 1.0) (fun (x, y) ->
               let x, y = x +. 0.5, y +. 0.5 in
               fill_circle (x, y) 10 ;
               x, y )
         and+ () = T.padding 0.5
         and+ child = layout3 ~depth child in
         draw_line parent child ; parent )
  | Bin (left, right) ->
      let tree =
        T.vert
          (let open T.Syntax in
           let+ parent =
             T.make (1.0, 1.0) (fun (x, y) ->
                 let x, y = x +. 0.5, y +. 0.5 in
                 fill_circle (x, y) 10 ;
                 x, y )
           and+ () = T.padding 0.5
           and+ left, right =
             T.horz
               (T.pair ~padding:0.5 (layout3 ~depth left) (layout3 ~depth right))
           in
           draw_line parent left ; draw_line parent right ; parent )
      in
      if depth <> -1
      then tree
      else
        let open T.Syntax in
        let+ ccc = T.contour tree and+ r = tree in
        G.set_color G.blue ;
        List.iter (fun ((x, y), (w, h)) -> draw_rect (x, y) (w, h)) ccc ;
        G.set_color G.black ;
        r

let layout3 t = layout3 ~depth:0 t

let tree0 = Bin (Bin (Leaf, Leaf), Leaf)

let tree1 = Bin (tree0, tree0)

let tree2 = Bin (tree1, tree0)

let tree3 = Bin (tree1, tree2)

let tree = Bin (tree3, Single (Bin (Leaf, Leaf)))

let () =
  G.open_graph " " ;
  G.set_line_width 3 ;
  let my_layout = layout ~depth:0 tree in
  let _, render = T.extract my_layout in
  let _ = render (6.0, 12.0) in
  let my_layout = layout3 tree in
  let _, render = T.extract my_layout in
  let _ = render (1.0, 1.0) in
  let my_layout2 = layout2 tree in
  let _, render2 = T.extract my_layout2 in
  let _ = render2 (20.0, 1.0) in
  ignore (G.read_key ()) ;
  G.close_graph ()
