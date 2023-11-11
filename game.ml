[@@@ocaml.warning "-21-32-69"]

open Js_of_ocaml
open Matter


module type Basket = sig
  val windowWidth : int
  val windowHeight : int
  val basketLeft : int
  val basketRight : int
  val edgeWidth : int
  val capY : int
end

(* Generate position data from window size *)
module MakeBasket
    (A : sig
       val windowWidth : int
       val windowHeight : int
     end) : Basket = struct
  include A
  let basketLeft = windowWidth*1/4
  let basketRight = windowWidth*3/4
  let edgeWidth = windowWidth*1/40
  let capY = windowHeight*1/20
end


type environment =
  { canvas : Dom_html.eventTarget Js.t
  ; basket : (module Basket)
  ; iEngine : < world : < > Js.prop > Js.t
  ; iRunner : < enabled : bool Js.prop > Js.t
  ; state : Random.State.t }


let capLabel = "cap"
let createBasket env =
  let (module Basket) = env.basket in
  let open Basket in
  let option = object%js val isStatic=true end in
  let ground =
    bodies##rectangle
      (windowWidth*1/2)
      (windowHeight*39/40)
      (basketRight - basketLeft + edgeWidth)
      (windowHeight*1/20)
      option in
  let wallLeft =
    bodies##rectangle
      basketLeft
      (windowHeight*1/2)
      edgeWidth
      (windowHeight*9/10)
      option in
  let wallRight =
    bodies##rectangle
      basketRight
      (windowHeight*1/2)
      edgeWidth
      (windowHeight*9/10)
      option in
  let cap =
    bodies##rectangle
      (windowWidth*1/2)
      (windowHeight*1/40)
      windowWidth
      (windowHeight*1/20)
      object%js
        val label = capLabel
        val isStatic = true
        val isSensor = true
        val render =
          object%js
            val visible = false
          end
      end in
  [| ground; wallLeft; wallRight; cap |]

type scene = Game | Over | WaitRetry

type t =
  { mutable nextBall : bodyClass Js.t option
  ; mutable ballGenerator : Dom_html.timeout_id_safe option
  ; mutable eventIds : Dom.event_listener_id list
  ; mutable mousePositionX : int
  ; mutable scene : scene
  ; mutable score : int
  ; env : environment
  }

module Balls = struct
  (* Ball data *)
  type t = { size : float; color : string; index : int; score : int }
  let make size color index score = { size; color; index; score }
  let ballArray =
    [|
      make 10. "#ff0000" 0 0;
      make 20. "#800080" 1 1;
      make 30. "#0000ff" 2 2;
      make 40. "#008080" 3 4;
      make 50. "#80ff00" 4 8;
      make 60. "#ff0000" 5 16;
      make 70. "#800080" 6 32;
      make 80. "#0000ff" 7 64;
      make 90. "#008080" 8 128;
      make 100. "#80ff00" 9 256;
      make 110. "#000000" 9 512;
    |]
  let max = Array.length ballArray
  let nth = Array.unsafe_get ballArray
end
module Label = struct
  type t = { index : int ; insert : bool }

  let to_string { index; insert } = Format.sprintf "%d,%d" index (if insert then 1 else 0)

  let of_string_opt str =
    let (let*) = Option.bind in
    match String.split_on_char ',' str with
    | [index; insert] ->
        let* index = int_of_string_opt index in
        let* insert = int_of_string_opt insert in
        let insert = if insert = 1 then true else false in
        Some {index; insert}
    | _ -> None

  let of_string str =
    of_string_opt str |> Option.get
end


let createBall ~preview (posX, posY) index =
  let { size; color; _ } : Balls.t = Balls.nth index in
  let option = object%js
    val isSleeping = preview
    val isSensor = preview
    val label = Label.to_string { index; insert=false }
    val restitution = 0.8
    val render = object%js
      val fillStyle = color
    end
  end in
  let ball = bodies##circle posX posY size option in
  let () =
    if preview then ()
    else ignore @@ Dom_html.setTimeout (fun () -> ball##.label := Label.to_string {index; insert=true}) 1000. in
  ball

let adjustBallPosition env (x, y) size =
  let (module Basket) = env.basket in
  let open Basket in
  let x = max x (float_of_int basketLeft +. float_of_int edgeWidth /. 2. +. size) in
  let x = min x (float_of_int basketRight -. float_of_int edgeWidth /. 2. -. size) in
  let y = min y (float_of_int capY -. size) in
  (x, y)

(* Generate index at random (Exponential distribution) *)
let nextIndex state =
  let nextIndex, _r =
    let r = Random.State.int state (1 lsl Balls.max) in
    let rec loop r n =
      if r > 0 then loop (r lsr 1) (n-1)
      else n
    in
    loop r Balls.max, r in
  nextIndex

(* Put in a new ball to the basket *)
let generateBall t =
  let { iEngine; state; basket=(module Basket); _ } = t.env in
  let pos_x = t.mousePositionX in
  let index = nextIndex state in
  let { size; _ } : Balls.t = Balls.nth index in
  let pos = adjustBallPosition t.env (float_of_int pos_x, float_of_int Basket.capY) size in
  let n = createBall ~preview:true pos index in
  composite##add iEngine##.world n;
  t.ballGenerator <- None;
  t.nextBall <- Some n

(* Add mouse events related to the game *)
let createMouseEvents t next_f =
  let { canvas=elem; _ } = t.env in
  let open Dom_html in
  let id0 =
    addEventListener
      elem
      Event.mousemove
      (handler
         (fun e ->
            (* Move the ball position horizontally following the mouse position *)
            (match t.nextBall with
             | None -> ()
             | Some n ->
                 (t.mousePositionX <- e##.clientX;
                  let x = t.mousePositionX in
                  let y = n##.position##.y in
                  let { size; _ } : Balls.t =
                    let { index; _ } : Label.t = Label.of_string n##.label in
                    Balls.nth index in
                  let (x, y) = adjustBallPosition t.env (float_of_int x, y) size in
                  body##setPosition n (Vector.create_float x y)));
            Js._false))
      Js._false in
  let id1 =
    addEventListener
      elem
      Event.mouseup
      (handler
         (fun _e ->
            (* Throw in the ball *)
            (match t.nextBall with
             | Some n ->
                 sleeping##set n false;
                 body##setSpeed n 0;
                 n##.isSensor := false;
                 let _ =
                   Dom_html.setTimeout
                     (fun () ->
                        let { index; _ } :Label.t = Label.of_string n##.label in
                        n##.label := Label.to_string { index; insert=true })
                     1000. in
                 next_f ()
             | None -> ());
            t.nextBall <- None;
            Js._false))
      Js._false in
  [id0; id1]

let addCollisionEvents t f_gameover =
  let { iEngine; _ } = t.env in
  events##on
    iEngine
    "collisionStart"
    (fun e ->
       (* Merge balls if the same ones are contacted *)
       let pairs = Js.to_array e##.pairs in
       Array.iter
         (fun p ->
            let optA = Label.of_string_opt p##.bodyA##.label in
            let optB = Label.of_string_opt p##.bodyB##.label in
            match optA, optB with
            | Some { index ; _ }, Some { index = indexB; _ }
              when index=indexB && index + 1 < Balls.max ->
                begin
                  let open Vector in
                  let posA = p##.bodyA##.position in
                  let posB = p##.bodyB##.position in
                  let posM = mult (add posA posB) 0.5 in
                  let velA = p##.bodyA##.velocity in
                  let velB = p##.bodyB##.velocity in
                  let velM = mult (add velA velB) 0.5 in
                  composite##remove iEngine##.world p##.bodyA;
                  composite##remove iEngine##.world p##.bodyB;
                  let n = createBall ~preview:false (posM##.x, posM##.y) (index+1) in
                  body##setVelocity n velM;
                  composite##add iEngine##.world n;
                  let { score=s; _ } : Balls.t = Balls.nth (index+1) in
                  t.score<-t.score + s
                end
            | _ -> ())
         pairs);
  events##on
    iEngine
    "collisionActive"
    (fun e ->
       (* Check balls are overflowing *)
       let pairs = Js.to_array e##.pairs in
       Array.iter
         (fun p ->
            let a, b =
              if p##.bodyA##.label = capLabel
              then p##.bodyB, p##.bodyA
              else p##.bodyA, p##.bodyB in
            if b##.label = capLabel then
              match Label.of_string_opt a##.label with
              | Some { insert=true; _ } -> f_gameover ()
              | Some _ | None -> ()
            else ())
         pairs);
  ()

(* End the game, stop moving balls, delete events, and transit to the next scene *)
let endGame t =
  let { iEngine; iRunner; _ } = t.env in
  t.scene <- Over;
  t.nextBall <- None;
  let () =
    match t.ballGenerator with
    | Some id ->
        Dom_html.clearTimeout id;
        t.ballGenerator <- None
    | None -> () in
  List.iter Dom_html.removeEventListener t.eventIds;
  t.eventIds <- [];
  events##off iEngine;
  iRunner##.enabled := false;
  let _ = (* Wait 2 seconds and transit scene *)
    Dom_html.setTimeout
      (fun () -> t.scene<-WaitRetry)
      2000. in
  ()

let reset t =
  let { iEngine; iRunner; _ } = t.env in
  resetEngine iEngine;
  iRunner##.enabled := true;
  t.score<-0

let rec startGame t =
  let { iEngine; _ } = t.env in
  generateBall t;
  let ids =
    createMouseEvents t
      (fun () ->
         let id = (* Wait 2 seconds and generate a ball *)
           Dom_html.setTimeout
             (fun () -> generateBall t)
             2000. in
         t.ballGenerator <- Some id) in
  t.eventIds <- ids;
  let basket = createBasket t.env in
  composite##add iEngine##.world basket;
  addCollisionEvents t (fun () -> endGame t);
  let _ =
    Dom_html.(
      addEventListener
        t.env.canvas
        Event.click
        (handler
           (fun _e ->
              (match t.scene with
               | WaitRetry ->
                   restartGame t;
                   t.scene <- Game
               | Game | Over -> ());
              Js._false))
        Js._false) in
  ()

and restartGame t =
  t.scene <- Game;
  reset t;
  startGame t

let make ~iEngine ~iRunner ~canvas ~windowWidth ~windowHeight ~state () =
  let module Basket = MakeBasket(struct let windowWidth=windowWidth let windowHeight=windowHeight end) in
  let env = { iEngine; iRunner; canvas; basket=(module Basket); state } in
  let mousePositionX = windowWidth/2 in
  { nextBall=None; ballGenerator=None; eventIds=[]; score=0; mousePositionX; scene=Game; env }

let draw t (ctxt : Dom_html.canvasRenderingContext2D Js.t) =
  let { basket=(module Basket); _ } = t.env in
  let open Basket in
  match t.scene with
  | Game -> (
      ctxt##.font := Js.string "24px serif";
      ctxt##.fillStyle := Js.string "#000000";
      ctxt##.textAlign := Js.string "center";
      ctxt##fillText
        (Js.string (Format.sprintf "Score: %d" (t.score)))
        (float_of_int windowWidth /. 8.)
        (float_of_int windowHeight /. 2.);)
  | Over ->
      (ctxt##.font := Js.string "50px serif";
       ctxt##.fillStyle := Js.string "#000000";
       ctxt##.textAlign := Js.string "center";
       ctxt##fillText
         (Js.string "GameOver")
         (float_of_int windowWidth /. 2.)
         (float_of_int windowHeight /. 2.))
  | WaitRetry ->
      (ctxt##.font := Js.string "50px serif";
       ctxt##.fillStyle := Js.string "#000000";
       ctxt##.textAlign := Js.string "center";
       ctxt##fillText
         (Js.string "GameOver")
         (float_of_int windowWidth /. 2.)
         (float_of_int windowHeight /. 2.);
       ctxt##fillText
         (Js.string "Click to retry")
         (float_of_int windowWidth /. 2.)
         (float_of_int windowHeight /. 2. +. 40.))
