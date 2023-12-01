(* Data like basket position and sizes *)
module type PositionData = sig
  val windowWidth : int
  val windowHeight : int
  val basketLeft : int
  val basketRight : int
  val basketBottomX : int
  val basketBottomY : int
  val basketBottomThickness : int
  val basketEdgeWidth : int
  val basketWallY : int
  val basketWallTall : int
  val capRealY : int
  val capThickness : int
  val capY : int
  val scoreLetterX : int
  val scoreLetterY : int
  val scoreX : int
  val scoreY : int
  val hierarchyLetterX : int
  val hierarchyLetterY : int
  val hierarchyBallX : int
  val hierarchyBallY : int
  val gameOverLetterX : int
  val gameOverLetterY : int
  val retryLetterX : int
  val retryLetterY : int
end

(* Generate position data from window size *)
module MakePositionData
    (A : sig
       val windowWidth : int
       val windowHeight : int
     end) : PositionData = struct
  include A
  let basketLeft = windowWidth * 1 / 4
  let basketRight = windowWidth * 3 / 4
  let basketBottomX = windowWidth / 2
  let basketBottomY = windowHeight * 39 / 40
  let basketBottomThickness = windowHeight / 20
  let basketEdgeWidth = windowWidth * 1 / 20
  let basketWallY = windowHeight / 2
  let basketWallTall = windowHeight * 9 / 10
  let capRealY = windowHeight * 1 / 80
  let capThickness = windowHeight * 1 / 20
  let capY = windowHeight * 1 / 20

  let scoreLetterX = windowWidth / 8
  let scoreLetterY = windowHeight * 4 / 20
  let scoreX = scoreLetterX
  let scoreY = scoreLetterY + windowHeight * 1 / 20
  let hierarchyLetterX = windowWidth * 7 / 8
  let hierarchyLetterY = windowHeight * 1 / 5
  let hierarchyBallX = hierarchyLetterX
  let hierarchyBallY = hierarchyLetterY + windowHeight * 1 / 10
  let gameOverLetterX = windowWidth / 2
  let gameOverLetterY = windowHeight * 9 / 20
  let retryLetterX = gameOverLetterX
  let retryLetterY = gameOverLetterY + windowHeight * 3 / 20
end
