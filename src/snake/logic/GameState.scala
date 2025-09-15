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
                      applePos : Point = Point(-1, -1),
                      gameEnd : Boolean = false
                    ){
  regenApple()

  // TODO implement me
  def getCellType(p: Point): CellType = {
    if (p==player.headPosition) SnakeHead(player.headFacing)
    else if (p==applePos) Apple()
    else Empty()
  }

  // TODO implement me
  def step() : GameState = {
    var nextGameEnd = gameEnd
    var nextApplePos: Point = applePos
    if (nextApplePos == Point(-1, -1)) nextGameEnd = true
    if(!nextGameEnd) {
      val nextHeadDir = queuedDir
      var nextHeadPos: Point = (player.headPosition + nextHeadDir.toVector).wrap(Point(0, 0), gridDims.toPoint - Point(1, 1))
      var nextGrowthRemaining: Int = player.growthRemaining
      var nextApplePos: Point = applePos
      var nextGameEnd = gameEnd

      if (nextHeadPos == applePos) {
        nextGrowthRemaining += 3
        nextApplePos = regenApple()
      }
      if (player.growthRemaining > 0) {
        nextGrowthRemaining -= 1


      }

      copy(
        player = player.copy(
          headFacing = nextHeadDir,
          headPosition = nextHeadPos,
          growthRemaining = nextGrowthRemaining
        ),
        applePos = nextApplePos,
        queuedDir = nextHeadDir,
        gameEnd = nextGameEnd
      )
    }
    else copy(gameEnd = nextGameEnd)
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


