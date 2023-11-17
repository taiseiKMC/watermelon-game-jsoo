open Js_of_ocaml

class type vector = object
  method x : float Js.prop
  method y : float Js.prop
end

class type vectorModule = object
  method create_int : int -> int -> vector Js.t Js.meth
  method create_float : float -> float -> vector Js.t Js.meth
  method add : vector Js.t -> vector Js.t -> vector Js.t Js.meth
  method mult : vector Js.t -> float -> vector Js.t Js.meth
end

class type engine = object
  method world : < > Js.prop (** Root composite instance *)
end

class type runner = object
  method enabled : bool Js.t Js.prop
end

(* Bind Js modules *)
let _Engine = Js.Unsafe.pure_js_expr "Matter.Engine"
let _Render = Js.Unsafe.pure_js_expr "Matter.Render"
let _Runner = Js.Unsafe.pure_js_expr "Matter.Runner"
let _Bodies = Js.Unsafe.pure_js_expr "Matter.Bodies"
let _Body = Js.Unsafe.pure_js_expr "Matter.Body"
let _Composite = Js.Unsafe.pure_js_expr "Matter.Composite"
let _Mouse = Js.Unsafe.pure_js_expr "Matter.Mouse"
let _MouseConstraint = Js.Unsafe.pure_js_expr "Matter.MouseConstraint"
let _Events = Js.Unsafe.pure_js_expr "Matter.Events"
let _Sleeping = Js.Unsafe.pure_js_expr "Matter.Sleeping"
let _Vector : vectorModule Js.t = Js.Unsafe.pure_js_expr "Matter.Vector"


(** Wrapper of vector module in JS *)
module Vector = struct
  (* Should extract all arguments because of the specification of Js_of_ocaml.
     (Js cannot do partial appications) *)
  let create_int : int -> int -> vector Js.t =
    fun x y -> _Vector##create_int x y
  let create_float : float -> float -> vector Js.t =
    fun x y -> _Vector##create_float x y

  let add : vector Js.t -> vector Js.t -> vector Js.t =
    fun p q ->
    _Vector##add p q

  let mult : vector Js.t -> float -> vector Js.t =
    fun p x ->
    _Vector##mult p x
end

class type body = object
  method position : vector Js.t Js.readonly_prop
  method velocity : vector Js.t Js.readonly_prop
  method isSensor : bool Js.prop
  method isSleeping : bool Js.readonly_prop
  method isStatic : bool Js.readonly_prop
  method label : string Js.prop
end

let resetEngine (engine : engine Js.t) =
  let () = _Composite##clear engine##.world in
  (* Trick to cause the event "afterRender" everytime *)
  _Composite##add
    engine##.world
    [| _Bodies##rectangle
         0 0 0 0
         object%js
           val isStatic = true
           val isSensor = true
         end |]
