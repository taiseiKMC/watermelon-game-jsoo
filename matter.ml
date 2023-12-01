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

class type body = object
  method position : vector Js.t Js.readonly_prop
  method velocity : vector Js.t Js.readonly_prop
  method isSensor : bool Js.prop
  method isSleeping : bool Js.readonly_prop
  method isStatic : bool Js.readonly_prop
  method label : string Js.prop
end

type composite (* phantom type *)

class type compositeModule = object
  method add : composite Js.t -> body Js.t Js.js_array Js.t -> unit Js.meth
  method remove : composite Js.t -> body Js.t Js.js_array Js.t -> unit Js.meth
  method clear : composite Js.t -> unit Js.meth
end

class type engine = object
  method world : composite Js.t Js.prop (** Root composite instance *)
end

class type runner = object
  method enabled : bool Js.t Js.prop
end


(* Bind Js modules *)
let _Matter = Js.Unsafe.pure_js_expr "Matter"
let _Engine = _Matter##.Engine
let _Render = _Matter##.Render
let _Runner = _Matter##.Runner
let _Bodies = _Matter##.Bodies
let _Body = _Matter##.Body
let _Composite : compositeModule Js.t = _Matter##.Composite
let _Mouse = _Matter##.Mouse
let _MouseConstraint = _Matter##.MouseConstraint
let _Events = _Matter##.Events
let _Sleeping = _Matter##.Sleeping
let _Vector : vectorModule Js.t = _Matter##.Vector


(** A wrapper of vector module in matter-js *)
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

  module Operator = struct
    let (+) = add
    let ( * ) = mult
  end
end

let resetEngine (engine : engine Js.t) =
  let () = _Composite##clear engine##.world in
  (* Trick to cause the event "afterRender" everytime *)
  _Composite##add
    engine##.world
    (Js.array
       [| _Bodies##rectangle
            0 0 0 0
            object%js
              val isStatic = true
              val isSensor = true
            end |])
