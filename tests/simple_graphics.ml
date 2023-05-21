module G = Graphics

let leaf_width = 30.0

let leaf_height = 15.0

let parent_width = 15.0

let parent_height = 30.0

let int = int_of_float

type t = Leaf | Bin of t * t

let rec my_layout = function
  | Leaf ->
      Prettree.make (leaf_width, leaf_height)
      @@ fun (x, y) ->
      (* draw a leaf at position (x, y) *)
      G.set_color G.black ;
      G.fill_rect (int x) (int y) (int leaf_width) (int leaf_height) ;
      (* return the leaf position such that we can draw edges to it later *)
      x +. (leaf_width /. 2.0), y
  | Bin (left, right) ->
      Prettree.vert
      @@
      (* parent is vertically over its children *)
      let open Prettree.Syntax in
      let+ parent_x, parent_y =
        Prettree.make (parent_width, parent_height)
        @@ fun (x, y) ->
        G.set_color G.blue ;
        G.fill_rect (int x) (int y) (int parent_width) (int parent_height) ;
        (* draw the parent node at (x, y) *)
        x +. (parent_width /. 2.0), y
      and+ () =
        (* vertical separation betwen parent and children *)
        Prettree.padding 10.0
      and+ (left_x, left_y), (right_x, right_y) =
        Prettree.horz
        @@ (* siblings are horizontally aligned *)
        let+ left_pos = my_layout left
        and+ () =
          (* horizontal separation between left and right *)
          Prettree.padding 10.0
        and+ right_pos = my_layout right in
        left_pos, right_pos (* return children position *)
      in
      (* draw edges from (parent_x, parent_y) to (left_x, left_y)
                and from (parent_x, parent_y) to (right_x, right_y)
      *)
      G.set_color G.black ;
      G.moveto (int parent_x) (int (parent_y +. parent_height)) ;
      G.lineto (int left_x) (int left_y) ;
      G.moveto (int parent_x) (int (parent_y +. parent_height)) ;
      G.lineto (int right_x) (int right_y) ;
      (* return the parent position *)
      parent_x +. (parent_width /. 2.0), parent_y

let test = Bin (Bin (Leaf, Leaf), Bin (Leaf, Bin (Leaf, Leaf)))

let () =
  G.open_graph " " ;
  G.set_line_width 2 ;
  (* compute our test tree layout *)
  let test_layout = my_layout test in
  (* extract the layout to get the bounding box and our rendering function *)
  let (_tree_width, _tree_height), my_render = Prettree.extract test_layout in
  (* call our rendering function at some initial position (0,0) *)
  let _root_x, _root_y = my_render (100.0, 100.0) in
  (* ... *)
  ignore (G.read_key ()) ;
  G.close_graph ()
