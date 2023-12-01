open Js_of_ocaml
open Matter

type t

val make :
  engine:engine Js.t ->
  runner:runner Js.t ->
  canvas:Dom_html.eventTarget Js.t ->
  windowWidth:int ->
  windowHeight:int ->
  state:Random.State.t ->
  unit ->
  t

val startGame : t -> unit

val draw : t -> Dom_html.canvasRenderingContext2D Js.t -> unit
