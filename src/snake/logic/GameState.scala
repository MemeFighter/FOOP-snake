package snake.logic

import engine.random.RandomGenerator
import scala.collection.mutable.ArrayBuffer

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
case class GameState(
                      random : RandomGenerator,
                      gridDims : Dimensions,
                      queuedDir : Direction = East(),
                      player : SnakeActor = SnakeActor(),
                      applePos : Point = Point(-1, -1),
                      gameEnd : Boolean = false
                    ){

  def init() : GameState = {
    copy(applePos = regenApple())
  }


  def getCellType(p: Point) : CellType = {
    if (p==player.headPosition) SnakeHead(player.headFacing)
    else if (player.segmentPositions.contains(p)){
      SnakeBody((player.segmentPositions.indexOf(p)+1).toFloat/player.length)
    }
    else if (p==applePos) Apple()
    else Empty()
  }

  def step() : GameState = {
    if (gameEnd) return copy()
    else if (applePos == Point(-1,-1)) return copy(gameEnd = true)

    var nextGameEnd : Boolean = gameEnd
    val nextHeadPos : Point = (player.headPosition + queuedDir.toVector).wrap(Point(0, 0), gridDims.toPoint - Point(1, 1))
    var nextApplePos : Point = applePos
    if (player.segmentPositions.dropRight(1).contains(nextHeadPos)) nextGameEnd = true

    val returning : GameState = copy(player = player.copy(headFacing = queuedDir).slither(nextHeadPos))
    var nextGrowthRemaining : Int = returning.player.growthRemaining
    if (nextHeadPos == applePos) {
      nextGrowthRemaining += 3
      nextApplePos = returning.regenApple()
    }
    returning.copy(
      player = returning.player.copy(growthRemaining = nextGrowthRemaining),
      applePos = nextApplePos,
      gameEnd = nextGameEnd
    )

  }

  private def freePoints : Vector[Point] = {
    val freePoints : ArrayBuffer[Point] = new ArrayBuffer[Point]
    for (tryPoint <- gridDims.allPointsInside){
      if(getCellType(tryPoint) == Empty()) freePoints += tryPoint
    }
    freePoints.toVector
  }

  private def regenApple() : Point = {
    val freePoints : Vector[Point] = freePoints
    if(freePoints.nonEmpty) freePoints(random.randomInt(freePoints.length))
    else Point(-1,-1)
  }

  def changeDir(d: Direction): GameState = copy(queuedDir = if(d != player.headFacing.opposite) d else queuedDir)

  def gameOver() : Boolean = gameEnd

}
