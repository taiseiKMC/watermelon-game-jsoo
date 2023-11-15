[@@@ocaml.warning "-21-32-69"]

open Js_of_ocaml
open Matter

let st = Random.State.make_self_init ()

let windowWidth = 800
let windowHeight = 600

let iEngine = engine##create ()

let iRunner = runner##create ()

let iRender =
  render##create
    (object%js
      val element = Dom_html.document##.body
      val engine = iEngine
      val options = object%js
        val width = windowWidth
        val height = windowHeight
        val wireframes = false
        val background = "white"
      end
    end)

let canvas = iRender##.canvas

type scene = Title | Game
let scene = ref Title

let game = Game.make ~iEngine ~iRunner ~canvas ~state:st ~windowWidth ~windowHeight ()

let draw () =
  resetEngine iEngine;
  events##on
    iRender
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
  render##run iRender;
  runner##run iRunner iEngine;
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
