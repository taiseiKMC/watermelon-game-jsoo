type texture

(** Data for each ball *)
type t = {size : float; color : texture; index : int; score : int}

(** The count of ball types *)
val max : int

(** [nth i] returns ball data [t] if 0 <= i < max *)
val nth : int -> t

(** Returns render for option of [body] *)
val render : texture -> 'a Js_of_ocaml.Js.t
