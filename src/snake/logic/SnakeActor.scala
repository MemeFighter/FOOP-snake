package snake.logic

import snake.logic

import scala.collection.mutable.ArrayBuffer



case class SnakeActor(
                       length : Int = 3,
                       growthRemaining : Int = 0,
                       headFacing : Direction = East(),
                       headPosition : Point = Point(2,0),
                       segmentPositions : Vector[Point] = Vector(Point(1,0), Point(0,0))
                     ) {
  def changeDir(d : Direction) : SnakeActor = copy(headFacing = if (d != headFacing.opposite) d else headFacing)
  def slither(p : Point) : SnakeActor = {
    var nextLength : Int = length
    var nextGrowthRemaining : Int = growthRemaining
    var nextPositions : Array[Point] = segmentPositions.toArray
    if (nextGrowthRemaining > 0){
      nextGrowthRemaining -= 1
      nextLength += 1
      nextPositions :+= Point(-1,-1)
    }
    for (i <- nextLength-2 to 1 by -1) nextPositions(i) = nextPositions(i-1)
    nextPositions(0) = headPosition
    copy(
      length = nextLength,
      growthRemaining = nextGrowthRemaining,
      headPosition = p,
      segmentPositions = nextPositions.toVector
    )

  }

}

