open Js_of_ocaml

type texture = Texture of string * float | Color of string

(* Ball data *)
type t = {size : float; color : texture; index : int; score : int}

let make color index score =
  let size = float_of_int @@ ((index * 10) + 10) in
  {size; color; index; score}

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
    make (Color "#800000") 3 4; (* dekopon *)
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

let render color =
  (* These object literals both can not be typed even if using optdef as its type
     because matter-js library checks the existence of properties by in-operator.
     in でプロパティの存在判定をしているため optdef ではだめだった... *)
  match color with
  | Color col ->
      Js.Unsafe.coerce
        (object%js
           val fillStyle = col
        end)
  | Texture (text, scale) ->
      Js.Unsafe.coerce
        (object%js
           val sprite =
             object%js
               val texture = text
               val xScale = scale
               val yScale = scale
             end
        end)
