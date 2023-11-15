open Js_of_ocaml

(* Bind Js modules *)
let engine = Js.Unsafe.pure_js_expr "Matter.Engine"
let render = Js.Unsafe.pure_js_expr "Matter.Render"
let runner = Js.Unsafe.pure_js_expr "Matter.Runner"
let bodies = Js.Unsafe.pure_js_expr "Matter.Bodies"
let body = Js.Unsafe.pure_js_expr "Matter.Body"
let composite = Js.Unsafe.pure_js_expr "Matter.Composite"
let mouse = Js.Unsafe.pure_js_expr "Matter.Mouse"
let mouseConstraint = Js.Unsafe.pure_js_expr "Matter.MouseConstraint"
let events = Js.Unsafe.pure_js_expr "Matter.Events"
let sleeping = Js.Unsafe.pure_js_expr "Matter.Sleeping"
let vector = Js.Unsafe.pure_js_expr "Matter.Vector"

class type vector = object
  method x : float Js.prop
  method y : float Js.prop
end

(** Wrapper of vector module in JS *)
module Vector = struct
  (* Should extract all arguments because of the specification of Js_of_ocaml.
     (Js cannot do partial appications) *)
  let create : int -> int -> vector Js.t =
    fun x y -> vector##create x y
  let create_float : float -> float -> vector Js.t =
    fun x y -> vector##create x y

  let add : vector Js.t -> vector Js.t -> vector Js.t =
    fun p q ->
    vector##add p q

  let mult : vector Js.t -> float -> vector Js.t =
    fun p x ->
    vector##mult p x
end

class type body = object
  method position : vector Js.t Js.readonly_prop
  method velocity : vector Js.t Js.readonly_prop
  method isSensor : bool Js.prop
  method label : string Js.prop
end

let resetEngine (iEngine : < world : < > Js.prop > Js.t) =
  let () = composite##clear iEngine##.world in
  (* Trick to cause the event "afterRender" everytime *)
  composite##add
    iEngine##.world
    [| bodies##rectangle
         0 0 0 0
         object%js
           val isStatic = true
           val isSensor = true
         end |]
