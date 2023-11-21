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
  val scoreLetterX : int
  val scoreLetterY : int
  val scoreX : int
  val scoreY : int
  val hierarchyLetterX : int
  val hierarchyLetterY : int
  val gameOverLetterX : int
  val gameOverLetterY : int
  val retryLetterX : int
  val retryLetterY : int
end

(* Generate position data from window size *)
module MakeBasket
    (A : sig
       val windowWidth : int
       val windowHeight : int
     end) : Basket = struct
  include A
  let basketLeft = windowWidth * 1 / 4
  let basketRight = windowWidth * 3 / 4
  let edgeWidth = windowWidth * 1 / 20
  let capY = windowHeight * 1 / 20

  let scoreLetterX = windowWidth / 8
  let scoreLetterY = windowHeight * 4 / 20
  let scoreX = scoreLetterX
  let scoreY = scoreLetterY + windowHeight * 1 / 20
  let hierarchyLetterX = windowWidth * 7 / 8
  let hierarchyLetterY = windowHeight * 1 / 5
  let gameOverLetterX = windowWidth / 2
  let gameOverLetterY = windowHeight * 8 / 20
  let retryLetterX = gameOverLetterX
  let retryLetterY = gameOverLetterY + windowHeight * 3 / 20
end


type environment =
  { canvas : Dom_html.eventTarget Js.t
  ; basket : (module Basket)
  ; engine : engine Js.t
  ; runner : runner Js.t
  ; state : Random.State.t }


let capLabel = "cap"
let createBasket env =
  let (module Basket) = env.basket in
  let open Basket in
  let option = object%js val isStatic=true end in
  let ground =
    _Bodies##rectangle
      (windowWidth * 1 / 2)
      (windowHeight * 39 / 40)
      (basketRight - basketLeft + edgeWidth)
      (windowHeight * 1 / 15)
      option in
  let wallLeft =
    _Bodies##rectangle
      basketLeft
      (windowHeight * 1 / 2)
      edgeWidth
      (windowHeight * 9 / 10)
      option in
  let wallRight =
    _Bodies##rectangle
      basketRight
      (windowHeight * 1 / 2)
      edgeWidth
      (windowHeight * 9 / 10)
      option in
  let cap =
    _Bodies##rectangle
      (windowWidth * 1 / 2)
      (windowHeight * 1 / 80)
      windowWidth
      (windowHeight * 1 / 20)
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
  { mutable nextBall : body Js.t option
  ; mutable ballGenerator : Dom_html.timeout_id_safe option
  ; mutable eventIds : Dom.event_listener_id list
  ; mutable mousePositionX : float
  ; mutable scene : scene
  ; mutable score : int
  ; env : environment
  }

module Balls = struct
  type texture = Texture of string * float | Color of string

  (* Ball data *)
  type t = { size : float; color : texture; index : int; score : int }

  let make color index score =
    let size = float_of_int @@ index * 10 + 10 in
    { size; color; index; score }

  let _ballArray = (* (discarded) texture *)
    [|
      make (Texture ("./resources/cherry.png", 2. *. 10. /. 145.)) 0 0; (* cherry *)
      make (Texture ("./resources/strawberry.png", 2. *. 20. /. 350.)) 1 1; (* strawberry *)
      make (Texture ("./resources/grape.png", 2. *. 30. /. 260.)) 2 2; (* grape *)
      make (Texture ("./resources/dekopon.png", 2. *. 40. /. 300.)) 3 4; (* dekopon *)
      make (Texture ("./resources/kaki.png", 2. *. 50. /. 350.)) 4 8; (* kaki *)
      make (Texture ("./resources/apple.png", 2. *. 60. /. 360.)) 5 16; (* apple *)
      make (Texture ("./resources/nashi.png", 2. *. 70. /. 340.)) 6 32; (* apple pear *)
      make (Texture ("./resources/peach.png", 2. *. 80. /. 310.)) 7 64; (* peach *)
      make (Texture ("./resources/pineapple.png", 2. *. 90. /. 180.)) 8 128; (* pineapple *)
      make (Texture ("./resources/melon.png", 2. *. 100. /. 340.)) 9 256; (* melon *)
      make (Texture ("./resources/watermelon.png", 2. *. 110. /. 320.)) 10 512; (* watermelon *)
    |]

  let ballArray =
    [|
      make (Color "#ff0000") 0 0; (* cherry *)
      make (Color "#00c0ff") 1 1; (* strawberry *)
      make (Color "#a839cb") 2 2; (* grape *)
      make (Color "#f6a03c") 3 4; (* dekopon *)
      make (Color "#ff6300") 4 8; (* kaki *)
      make (Color "#3639ff") 5 16; (* apple *)
      make (Color "#e3bf45") 6 32; (* apple pear *)
      make (Color "#e47ec3") 7 64; (* peach *)
      make (Color "#28f4e5") 8 128; (* pineapple *)
      make (Color "#4fe13e") 9 256; (* melon *)
      make (Color "#0e8e00") 10 512; (* watermelon *)
    |]
  let max = Array.length ballArray
  let nth = Array.unsafe_get ballArray
end

(** [label] is a kind of id used by 'body's in matter-js.
    [Label] provides the conversion between [Label.t] and [label] *)
module Label = struct
  type t =
    { index : int
    ; insert : bool (* Whether it's been several seconds since putting the ball in or not *)
    ; alive : bool (* The ball is removed or not (To remove a simultaneous collision) *)
    }

  let int_of_bool flag = if flag then 1 else 0

  let bool_of_int flag = if flag = 1 then true else false

  let to_string { index; insert; alive } = Format.sprintf "%d,%d,%d" index (int_of_bool insert) (int_of_bool alive)

  let of_string_opt str =
    let (let*) = Option.bind in
    match String.split_on_char ',' str with
    | [index; insert; alive] ->
        let* index = int_of_string_opt index in
        let* insert = int_of_string_opt insert in
        let insert = bool_of_int insert in
        let* alive = int_of_string_opt alive in
        let alive = bool_of_int alive in
        Some {index; insert; alive}
    | _ -> None

  let of_string str =
    of_string_opt str |> Option.get
end


let createBall ~preview (posX, posY) index : body Js.t =
  let { size; color; _ } : Balls.t = Balls.nth index in
  let render =
    (* These object literals both can not be typed even if using optdef as its type
       because matter-js library checks the existence of properties by in-operator.
       in でプロパティの存在判定をしているため optdef ではだめだった... *)
    match color with
    | Color col -> Js.Unsafe.coerce object%js val fillStyle = col end
    | Texture (text, scale) ->
        Js.Unsafe.coerce
          object%js
            val sprite =
              object%js
                val texture = text
                val xScale = scale
                val yScale = scale
              end
          end
  in

  let option = object%js
    val isSleeping = preview
    val isSensor = preview
    val label = Label.to_string { index; insert=false; alive=true }
    val restitution = 0.9
    val render = render
  end in
  let ball = _Bodies##circle posX posY size option in
  let () =
    if preview then ()
    else ignore @@ Dom_html.setTimeout (fun () -> ball##.label := Label.to_string {index; insert=true; alive=true}) 1000. in
  ball

(* Return the valid ball position, s.t. above basket between walls *)
let adjustBallPosition env (x, y) size =
  let (module Basket) = env.basket in
  let open Basket in
  let x = max x (float_of_int basketLeft +. float_of_int edgeWidth /. 2. +. size) in
  let x = min x (float_of_int basketRight -. float_of_int edgeWidth /. 2. -. size) in
  let y = min y (float_of_int capY -. size) in
  (x, y)

(* Exponential distribution *)
let _nextIndex0 state =
  let nextIndex, _r =
    let r = Random.State.int state ((1 lsl Balls.max) - 1) + 1 in
    let rec loop r n =
      if r > 0 then loop (r lsr 1) (n-1)
      else n
    in
    loop r Balls.max, r in
  nextIndex

(* Uniform distribution [0, 4] *)
let _nextIndex1 state =
  Random.State.int state 5

(* Generate index at random *)
let nextIndex = _nextIndex1

(* Put in a new ball to the basket *)
let generateBall t =
  let { engine; state; basket=(module Basket); _ } = t.env in
  let pos_x = t.mousePositionX in
  let index = nextIndex state in
  let { size; _ } : Balls.t = Balls.nth index in
  let pos = adjustBallPosition t.env (pos_x, float_of_int Basket.capY) size in
  let n = createBall ~preview:true pos index in
  _Composite##add engine##.world (Js.array [| n |]);
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
            (match t.nextBall, Js.Opt.to_option e##.target with
             | Some n, Some tar ->
                 ((* The way to get relative mouse position :
                     https://cpplover.blogspot.com/2009/06/dom-level-3.html *)
                   let rect = tar##getBoundingClientRect in
                   t.mousePositionX <- float_of_int e##.clientX -. rect##.left;
                   let x = t.mousePositionX in
                   let y = n##.position##.y in
                   let { size; _ } : Balls.t =
                     let { index; _ } : Label.t = Label.of_string n##.label in
                     Balls.nth index in
                   let (x, y) = adjustBallPosition t.env (x, y) size in
                   _Body##setPosition n (Vector.create_float x y))

             | _ -> ());
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
                 _Sleeping##set n false;
                 _Body##setSpeed n 0;
                 n##.isSensor := false;
                 let _ =
                   Dom_html.setTimeout
                     (fun () ->
                        let { index; _ } :Label.t = Label.of_string n##.label in
                        n##.label := Label.to_string { index; insert=true; alive=true })
                     1000. in
                 next_f ()
             | None -> ());
            t.nextBall <- None;
            Js._false))
      Js._false in
  [id0; id1]

let addCollisionEvents t f_gameover =
  let { engine; _ } = t.env in
  _Events##on
    engine
    "collisionStart"
    (fun e ->
       (* Merge balls if the same ones are contacted *)
       let pairs = Js.to_array e##.pairs in
       Array.iter
         (fun p ->
            let a : body Js.t = p##.bodyA in
            let b : body Js.t = p##.bodyB in
            let optA = Label.of_string_opt a##.label in
            let optB = Label.of_string_opt b##.label in
            match optA, optB with
            | Some ({ index ; alive=true; _ } as la), Some ({ index = indexB; alive=true; _ } as lb)
              when index = indexB && index + 1 < Balls.max ->
                begin
                  let open Vector in
                  let posA = a##.position in
                  let posB = b##.position in
                  let posM = mult (add posA posB) 0.5 in
                  let velA = a##.velocity in
                  let velB = b##.velocity in
                  let velM = mult (add velA velB) 0.5 in
                  a##.label := Label.to_string {la with alive=false};
                  b##.label := Label.to_string {lb with alive=false};
                  _Composite##remove engine##.world (Js.array [| a; b |]);
                  let n = createBall ~preview:false (posM##.x, posM##.y) (index + 1) in
                  _Body##setVelocity n velM;
                  _Composite##add engine##.world (Js.array [| n |]);
                  let { score = s; _ } : Balls.t = Balls.nth (index + 1) in
                  t.score<-t.score + s
                end
            | _ -> ())
         pairs);
  _Events##on
    engine
    "collisionActive"
    (fun e ->
       (* Check balls are overflowing. If so, call [f_gameover] *)
       let pairs = Js.to_array e##.pairs in
       Array.iter
         (fun p ->
            let (a : body Js.t), (b : body Js.t) = p##.bodyA, p##.bodyB in
            let a, b =
              if a##.label = capLabel
              then b, a else a, b in
            if b##.label = capLabel then
              match Label.of_string_opt a##.label with
              | Some { insert=true; _ } -> f_gameover ()
              | Some _ | None -> ()
            else ())
         pairs);
  ()

(* Add static balls with fixed size for displaying information *)
let addHierarchy t =
  let { basket=(module Basket); engine; _ } = t.env in
  let createBall (posX, posY) size index =
    let { color; _ } : Balls.t = Balls.nth index in
    let render =
      match color with
      | Color col -> Js.Unsafe.coerce object%js val fillStyle = col end
      | Texture (text, scale) ->
          Js.Unsafe.coerce
            object%js
              val sprite =
                object%js
                  val texture = text
                  val xScale = scale
                  val yScale = scale
                end
            end
    in
    let option = object%js
      val isSensor = true
      val isStatic = true
      val render = render
    end in
    _Bodies##circle posX posY size option in
  let rec f i =
    if i < Balls.max then
      let ball = createBall (Basket.windowWidth*7/8, Basket.windowHeight*3/10 + i * 40) 15 i in
      let () = _Composite##add engine##.world ball in
      f (i+1)
    else () in
  f 0

(* End the game, stop moving balls, delete events, and transit to the next scene *)
let endGame t =
  let { engine; runner; _ } = t.env in
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
  _Events##off engine;
  runner##.enabled := Js._false;
  let _ = (* Wait 2 seconds and transit scene *)
    Dom_html.setTimeout
      (fun () -> t.scene<-WaitRetry)
      2000. in
  ()

let reset t =
  let { engine; runner; _ } = t.env in
  resetEngine engine;
  runner##.enabled := Js._true;
  t.score<-0

let rec startGame t =
  let { engine; _ } = t.env in
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
  _Composite##add engine##.world (Js.array basket);
  addHierarchy t;
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

let make ~engine ~runner ~canvas ~windowWidth ~windowHeight ~state () =
  let module Basket = MakeBasket(struct let windowWidth=windowWidth let windowHeight=windowHeight end) in
  let env = { engine; runner; canvas; basket=(module Basket); state } in
  let mousePositionX = float_of_int @@ windowWidth/2 in
  { nextBall=None; ballGenerator=None; eventIds=[]; score=0; mousePositionX; scene=Game; env }

let draw t (ctxt : Dom_html.canvasRenderingContext2D Js.t) =
  let { basket=(module Basket); _ } = t.env in
  let open Basket in
  ctxt##.font := Js.string (Format.sprintf "%dpx serif" (windowHeight/20));
  ctxt##.fillStyle := Js.string "#000000";
  ctxt##.textAlign := Js.string "center";
  ctxt##fillText
    (Js.string (Format.sprintf "Score"))
    (float_of_int scoreLetterX)
    (float_of_int scoreLetterY);
  ctxt##fillText
    (Js.string (Format.sprintf "%d" (t.score)))
    (float_of_int scoreX)
    (float_of_int scoreY);
  ctxt##fillText
    (Js.string (Format.sprintf "Hierarchy"))
    (float_of_int hierarchyLetterX)
    (float_of_int hierarchyLetterY);
  match t.scene with
  | Game -> ()
  | Over ->
      ( ctxt##.font := Js.string (Format.sprintf "%dpx serif" (windowHeight/10));
        ctxt##fillText
          (Js.string "GameOver")
          (float_of_int gameOverLetterX)
          (float_of_int gameOverLetterY))
  | WaitRetry ->
      ( ctxt##.font := Js.string (Format.sprintf "%dpx serif" (windowHeight/10));
        ctxt##fillText
          (Js.string "GameOver")
          (float_of_int gameOverLetterX)
          (float_of_int gameOverLetterY);
        ctxt##.font := Js.string (Format.sprintf "%dpx serif" (windowHeight/20));
        ctxt##fillText
          (Js.string "Click to retry")
          (float_of_int retryLetterX)
          (float_of_int retryLetterY))
