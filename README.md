![binary trees](https://art-w.github.io/prettree/bin.png)

A small library to layout trees vertically in a pretty way: **[online documentation](https://art-w.github.io/prettree/prettree/Prettree)**
- minimal width and height
- parent centered above its children
- nodes and leaves can have different sizes
- custom padding between each subtrees

Note that this library doesn't handle drawing the tree, it only computes the ideal position of every nodes and leaves!  You will need to define a function which traverses your custom tree, building a layout and its rendering function:

```ocaml
type t = Leaf | Bin of t * t

let rec my_layout = function
  | Leaf ->
      Prettree.make (leaf_width, leaf_height) @@ fun (x, y) ->
      (* TODO: draw a leaf at position (x, y) *)
      (x, y) (* return the leaf position such that we can draw edges to it later *)
  | Bin (left, right) ->
      Prettree.vert @@ (* parent is vertically over its children *)
      let open Prettree.Syntax in
      let+ (parent_x, parent_y) =
        Prettree.make (parent_width, parent_height) @@ fun (x, y) ->
        (* TODO: draw the parent node at (x, y) *)
        (x, y)
      and+ () = Prettree.padding 1.0 (* vertical separation betwen parent and children *)
      and+ (left_x, left_y), (right_x, right_y) =
        Prettree.horz @@ (* siblings are horizontally aligned *)
        let+ left_pos = my_layout left
        and+ () = Prettree.padding 1.0 (* horizontal separation between left and right *)
        and+ right_pos = my_layout right
        in
        left_pos, right_pos (* return children positions *)
      in
      (* TODO: draw edges from (parent_x, parent_y) to (left_x, left_y)
                      and from (parent_x, parent_y) to (right_x, right_y)
      *)
      (parent_x, parent_y) (* return the parent position *)

let () =
  (* compute our test tree layout *)
  let test_layout = my_layout (Bin (Leaf, Leaf)) in
  (* extract the layout to get the bounding box and our rendering function *)
  let (tree_width, tree_height), my_render = Prettree.extract test_layout in
  (* call our rendering function at some initial position (0,0) *)
  let (root_x, root_y) = my_render (0.0, 0.0) in
  (* ... *)
```

![varying node height and width](https://art-w.github.io/prettree/wh.png)

The `tests/` folder contains variations using `Graphics` to render the trees, and also unicode art:

```
                         ┏━━━━━━━━━━━┓
                         ┃ fibonacci ┃
                         ┗━━━━━┯━━━━━┛
                               │
                            ┏━━┷━┓
                            ┃ 13 ┃
                            ┗━━┯━┛
          ┌────────────────────┴──────────────────┐
        ┏━┷━┓                                   ┏━┷━┓
        ┃ 5 ┃                                   ┃ 8 ┃
        ┗━┯━┛                                   ┗━┯━┛
   ┌──────┴───────┐                   ┌───────────┴───────────┐
 ┏━┷━┓          ┏━┷━┓               ┏━┷━┓                   ┏━┷━┓
 ┃ 2 ┃          ┃ 3 ┃               ┃ 3 ┃                   ┃ 5 ┃
 ┗━┯━┛          ┗━┯━┛               ┗━┯━┛                   ┗━┯━┛
┌──┴──┐       ┌───┴────┐          ┌───┴────┐           ┌──────┴───────┐
┷   ┏━┷━┓   ┏━┷━┓    ┏━┷━┓      ┏━┷━┓    ┏━┷━┓       ┏━┷━┓          ┏━┷━┓
1   ┃ 1 ┃   ┃ 1 ┃    ┃ 2 ┃      ┃ 1 ┃    ┃ 2 ┃       ┃ 2 ┃          ┃ 3 ┃
    ┗━┯━┛   ┗━┯━┛    ┗━┯━┛      ┗━┯━┛    ┗━┯━┛       ┗━┯━┛          ┗━┯━┛
    ┌─┴─┐   ┌─┴─┐   ┌──┴──┐     ┌─┴─┐   ┌──┴──┐     ┌──┴──┐       ┌───┴────┐
    ┷   ┷   ┷   ┷   ┷   ┏━┷━┓   ┷   ┷   ┷   ┏━┷━┓   ┷   ┏━┷━┓   ┏━┷━┓    ┏━┷━┓
    0   1   0   1   1   ┃ 1 ┃   0   1   1   ┃ 1 ┃   1   ┃ 1 ┃   ┃ 1 ┃    ┃ 2 ┃
                        ┗━┯━┛               ┗━┯━┛       ┗━┯━┛   ┗━┯━┛    ┗━┯━┛
                        ┌─┴─┐               ┌─┴─┐       ┌─┴─┐   ┌─┴─┐   ┌──┴──┐
                        ┷   ┷               ┷   ┷       ┷   ┷   ┷   ┷   ┷   ┏━┷━┓
                        0   1               0   1       0   1   0   1   1   ┃ 1 ┃
                                                                            ┗━┯━┛
                                                                            ┌─┴─┐
                                                                            ┷   ┷
                                                                            0   1
```
