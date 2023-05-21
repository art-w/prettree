type v2 = float * float

type 'a t
(** The type of tree layouts. *)

val make : v2 -> (v2 -> 'a) -> 'a t
(** [make (width, height) (fun (x, y) -> ...)] is a rectangle of size [width, height]. *)

val extract : 'a t -> v2 * (v2 -> 'a)
(** [extract t] returns the [(width, height)] bounding box and the rendering function of the layout [t]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map fn t] transforms the outcome of the layout [t] by [fn]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 fn t0 t1] concatenates the layouts [t0] and [t1] according to the surrounding [horz] or [vert]. The function [fn] is used to merge their outcome. *)

val horz : 'a t -> 'a t
(** [horz t] renders the subtrees of [t] horizontally. *)

val vert : 'a t -> 'a t
(** [vert t] renders the subtrees of [t] vertically. *)

val padding : float -> unit t
(** [padding size] adds empty space to separate subtrees. *)

val pair : ?padding:float -> 'a t -> 'b t -> ('a * 'b) t
(** [pair t0 t1] is the same as [map (fun a b -> a, b) t0 t1]. The optional [padding] between [t0] and [t1] defaults to [0.0]. *)

val list : ?padding:float -> 'a t list -> 'a list t
(** [list ts] is the concatenation of the layouts [ts], optionally with padding in between. *)

val box : 'a t -> 'a t
(** [box t] is the rectangular layout of [t] induced by its bounding box. *)

val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
(** [fn <$> t] is the same as [map fn t]. *)

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
(** [tf <*> tx] is the same as [map2 (fun f x -> f x) tf tx]. *)

module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

(** {2 Debugging} *)

val size : 'a t -> (v2 * v2) t
(** [size t] returns the bounding box of [t]. *)

val contour : 'a t -> (v2 * v2) list t
(** [contour t] returns the list of boxes defining the shape of [t]. *)
