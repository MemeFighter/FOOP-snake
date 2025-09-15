package snake.logic

import engine.random.RandomGenerator
import scala.collection.mutable.ArrayBuffer

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
case class GameState(
                      random: RandomGenerator,
                      gridDims : Dimensions,
                      queuedDir : Direction = East(),
                      player : SnakeActor = SnakeActor(),
                      applePos : Point = Point(6, 8),
                      gameEnd : Boolean = false
                    ){
  regenApple()

  // TODO implement me
  def getCellType(p: Point): CellType = {
    if (p==player.headPosition) SnakeHead(player.headFacing)
    else if (player.segmentPositions.contains(p)){
      var hue : Float = 0f
      for (i <- player.segmentPositions.indices){
        if (p == player.segmentPositions(i)) hue = (i+1).toFloat/player.length
      }
      SnakeBody(hue)
    }
    else if (p==applePos) Apple()
    else Empty()
  }

  // TODO implement me
  def step() : GameState = {
    if (gameEnd) copy()
    else {
      var nextGameEnd : Boolean = gameEnd
      var nextPlayer : SnakeActor = player

      val nextHeadDir : Direction = queuedDir
      val nextHeadPos : Point = (player.headPosition + nextHeadDir.toVector).wrap(Point(0, 0), gridDims.toPoint - Point(1, 1))

      var nextGrowthRemaining : Int = player.growthRemaining
      var nextApplePos : Point = applePos

      if (nextHeadPos == applePos) {
        nextGrowthRemaining += 3
        nextApplePos = regenApple()
      }
      else if (player.segmentPositions.contains(nextHeadPos)) nextGameEnd = true
      nextPlayer = nextPlayer.copy(headFacing = nextHeadDir, growthRemaining = nextGrowthRemaining).slither(nextHeadPos)
      println(""+nextPlayer.headPosition+" "+nextPlayer.segmentPositions)
      copy(
        player = nextPlayer,
        applePos = nextApplePos,
        queuedDir = nextHeadDir,
        gameEnd = nextGameEnd
      )
    }
  }

  def getFreePoints(): Vector[Point] = {
    val freePoints : ArrayBuffer[Point] = new ArrayBuffer[Point]
    for (i <- 0 until gridDims.width){
      for (j <- 0 until gridDims.height){
        if(getCellType(Point(i,j)) == Empty()) freePoints += Point(i,j)
      }
    }
    freePoints.toVector
  }

  def regenApple(): Point = {
    val freePoints : Vector[Point] = getFreePoints()
    if(freePoints.nonEmpty) freePoints(random.randomInt(freePoints.length))
    else Point(-1,-1)
  }
  // TODO implement me
  def changeDir(d: Direction): GameState = copy(queuedDir = if(d != player.headFacing.opposite) d else queuedDir)

  def gameOver : Boolean = gameEnd

}
