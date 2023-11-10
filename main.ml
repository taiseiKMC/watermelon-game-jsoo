[@@@ocaml.warning "-21-32-69"]

open Js_of_ocaml
open Matter


let iEngine = engine##create ()

let iRunner = runner##create ()

let windowWidth = 800
let windowHeight = 600

let st = Random.State.make_self_init ()

(* let docbody = Dom_html.document##.body *)
(* let canvas =
   let c = Dom_html.createCanvas Dom_html.document in
   (* let c = Option.get @@ Dom_html.getElementById_coerce "canvas-area" Dom_html.CoerceTo.canvas in *)
   c##.width := windowWidth;
   c##.height := windowHeight;
   Graphics_js.open_canvas c;
   c##.id := Js.string "canvas-";
   c##.style##.backgroundColor := Js.string "transparent";
   Dom.appendChild Dom_html.document##.body c;
   c *)

type scene = Title | Game | Over | WaitRetry
let scene = ref Title

let iRender =
  render##create
    (object%js
      val element = Dom_html.document##.body (* Dom_html.getElementById "canvas-" *)
      val engine = iEngine
      val options = object%js
        val width = windowWidth
        val height = windowHeight
        val wireframes = false
        val background = "white"
      end
    end)

let canvas = iRender##.canvas

(* let iMouse = mouse##create docbody
   let iMouseConstraint =
   mouseConstraint##create
    iEngine
      object%js
        val mouse = iMouse
      end

   let () = iRender##.mouse := iMouse *)

module Game = struct
  let basketLeft = windowWidth*1/4
  let basketRight = windowWidth*3/4
  let edgeWidth = windowWidth*1/40

  let coverLabel = "cover"
  let addBasket () =
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
    let cover =
      bodies##rectangle
        (windowWidth*1/2)
        (windowHeight*1/40)
        windowWidth
        (windowHeight*1/20)
        object%js
          val label = coverLabel
          val isStatic = true
          val isSensor = true
          val render =
            object%js
              val visible = false
            end
        end in
    composite##add iEngine##.world [| ground; wallLeft; wallRight; cover |]


  let nextBall : bodyClass Js.t option ref = ref None
  let ballGenerator = ref None
  let eventIds = ref []
  let score = ref 0


  type ballKind = { size : float; color : string; index : int; score : int }
  let makeKind size color index score = { size; color; index; score }
  let ballArray =
    [|
      makeKind (10. *. 3.) "#ff0000" 0 0;
      makeKind (20. *. 3.) "#800080" 1 1;
      makeKind (30. *. 3.) "#0000ff" 2 2;
      makeKind (40. *. 3.) "#008080" 3 4;
      makeKind (50. *. 3.) "#80ff00" 4 8;
      makeKind (60. *. 3.) "#ff0000" 5 16;
      makeKind (70. *. 3.) "#800080" 6 32;
      makeKind (80. *. 3.) "#0000ff" 7 64;
      makeKind (90. *. 3.) "#008080" 8 128;
      makeKind 100. "#80ff00" 9 256;
      makeKind 110. "#000000" 9 512;
    |]

  let maxIndex = Array.length ballArray

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
    let { size; color; _ } = Array.get ballArray index in
    let option = object%js
      val isSleeping = preview
      val isSensor = preview
      val label = Label.to_string { index; insert=false }
      val restitution = 0.8
      val render = object%js
        val fillStyle = color
      end
    end in
    let posX = max posX (float_of_int basketLeft +. float_of_int edgeWidth /. 2. +. size) in
    let posX = min posX (float_of_int basketRight -. float_of_int edgeWidth /. 2. -. size) in
    let ball = bodies##circle posX posY size option in
    let () =
      if preview then ()
      else ignore @@ Dom_html.setTimeout (fun () -> ball##.label := Label.to_string {index; insert=true}) 1000. in
    ball

  let mousePositionX = ref (windowWidth/2)

  let generateBall () =
    let id =
      Dom_html.setTimeout
        (fun () ->
           let pos_x = !mousePositionX in
           let nextIndex, _r =
             let r = Random.State.int st (1 lsl maxIndex) in
             let rec loop r n =
               if r > 0 then loop (r lsr 1) (n-1)
               else n
             in
             loop r (maxIndex), r in
           let n = createBall ~preview:true (float_of_int pos_x, 20.) nextIndex in
           composite##add iEngine##.world n;
           ballGenerator := None;
           nextBall := Some n)
        2000. in
    ballGenerator := Some id

  let addMouseEvents () =
    let open Dom_html in
    let id0 =
      addEventListener
        canvas
        Event.mousemove
        (handler
           (fun e ->
              (match !nextBall with
               | None -> ()
               | Some n ->
                 (mousePositionX := e##.clientX;
                  let x = !mousePositionX in
                  let { size; _ } =
                    let { index; _ } : Label.t = Label.of_string n##.label in
                    Array.unsafe_get ballArray index in
                  if (basketLeft + edgeWidth /2 < x - int_of_float size
                      && x + int_of_float size < basketRight - edgeWidth/2) then
                    body##setPosition n (Vector.create x 20)));
              Js._false))
        Js._false in
    let id1 =
      addEventListener
        canvas
        Event.mouseup
        (handler
           (fun _e ->
              (match !nextBall with
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
                 generateBall ()
               | None -> ());
              nextBall := None;
              Js._false))
        Js._false in
    [id0; id1]

  let rec collide () =
    events##on
      iEngine
      "collisionStart"
      (fun e ->
         let pairs = Js.to_array e##.pairs in
         Array.iter
           (fun p ->
              let optA = Label.of_string_opt p##.bodyA##.label in
              let optB = Label.of_string_opt p##.bodyB##.label in
              match optA, optB with
              | Some { index ; _ }, Some { index = indexB; _ }
                when index=indexB && index + 1 < maxIndex ->
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
                  let { score=s; _ } = Array.get ballArray (index+1) in
                  score:=!score + s
                end
              | _ -> ())
           pairs);
    events##on
      iEngine
      "collisionActive"
      (fun e ->
         let pairs = Js.to_array e##.pairs in
         Array.iter
           (fun p ->
              let a, b =
                if p##.bodyA##.label = coverLabel
                then p##.bodyB, p##.bodyA
                else p##.bodyA, p##.bodyB in
              if b##.label = coverLabel then
                match Label.of_string_opt a##.label with
                | Some { insert=true; _ } ->
                  Format.printf "GameOver@.";
                  endGame()
                | Some _ | None -> ()
              else ())
           pairs);
    ()

  and endGame () =
    scene := Over;
    nextBall := None;
    let () =
      match !ballGenerator with
      | Some id ->
        Dom_html.clearTimeout id;
        ballGenerator := None
      | None -> () in
    List.iter Dom_html.removeEventListener !eventIds;
    eventIds := [];
    events##off iEngine;
    iRunner##.enabled := false;
    let _ =
      Dom_html.setTimeout
        (fun () -> scene:=WaitRetry)
        2000. in
    ()

  let startGame () =
    scene := Game;
    composite##clear iEngine##.world;
    iRunner##.enabled := true;
    score:=0;
    generateBall ();
    let ids = addMouseEvents () in
    eventIds := ids;
    addBasket ();
    collide ()
end


let draw () =
  let open Dom_html in
  (* Trick to cause the event "afterRender" everytime *)
  composite##add
    iEngine##.world
    [| bodies##rectangle
         0 0 0 0
         object%js
           val isStatic = true
           val isSensor = true
         end |];
  let _ =
    addEventListener
      canvas
      Event.click
      (handler
         (fun _e ->
            (match !scene with
             | Title | WaitRetry ->
               Game.startGame ()
             | Game | Over -> ());
            Js._false))
      Js._false in
  events##on
    iRender
    "afterRender"
    (fun _e ->
       let ctxt : Dom_html.canvasRenderingContext2D Js.t =
         canvas##getContext Dom_html._2d_ in
       match !scene with
       | Game -> (
           ctxt##.font := Js.string "24px serif";
           ctxt##.fillStyle := Js.string "#000000";
           ctxt##.textAlign := Js.string "center";
           ctxt##fillText
             (Js.string (Format.sprintf "Score: %d" (!Game.score)))
             (float_of_int windowWidth /. 8.)
             (float_of_int windowHeight /. 2.);)
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
            (float_of_int windowHeight /. 2. +. 40.))
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
            (float_of_int windowHeight /. 2. +. 40.)))

let () =
  render##run iRender;
  runner##run iRunner iEngine;
  draw ()
