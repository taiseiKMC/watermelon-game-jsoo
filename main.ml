[@@@ocaml.warning "-21-32-69"]

open Js_of_ocaml
open Matter

let st = Random.State.make_self_init ()

let windowWidth = 800
let windowHeight = 600

let engine = _Engine##create ()

let runner = _Runner##create ()

let render =
  _Render##create
    (object%js
      val element = Dom_html.document##.body
      val engine = engine
      val options = object%js
        val width = windowWidth
        val height = windowHeight
        val wireframes = false
        val background = "white"
      end
    end)

let canvas = render##.canvas

type scene = Title | Game
let scene = ref Title

let game = Game.make ~engine ~runner ~canvas ~state:st ~windowWidth ~windowHeight ()

let draw () =
  resetEngine engine;
  _Events##on
    render
    "afterRender"
    (fun _e -> (* Expect to call this every frame *)
       let ctxt : Dom_html.canvasRenderingContext2D Js.t =
         canvas##getContext Dom_html._2d_ in
       match !scene with
       | Game -> Game.draw game ctxt
       | Title ->
           (ctxt##.font := Js.string "50px serif";
            ctxt##.fillStyle := Js.string "#000000";
            ctxt##.textAlign := Js.string "center";
            ctxt##fillText
              (Js.string "スイカゲームもどき")
              (float_of_int windowWidth /. 2.)
              (float_of_int windowHeight /. 2.);
            ctxt##fillText
              (Js.string "Click to start")
              (float_of_int windowWidth /. 2.)
              (float_of_int windowHeight /. 2. +. 40.)))

let () =
  _Render##run render;
  _Runner##run runner engine;
  let _ = (* Event to start the game by a click *)
    Dom_html.(
      addEventListener
        canvas
        Event.click
        (handler
           (fun _e ->
              (match !scene with
               | Title ->
                   Game.startGame game;
                   scene := Game
               | Game -> ());
              Js._false))
        Js._false) in
  draw ()
