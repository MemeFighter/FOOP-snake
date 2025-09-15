package snake.logic

case class SnakeActor(
                       length : Int = 3,
                       growthRemaining : Int = 0,
                       headFacing : Direction = East(),
                       headPosition : Point = Point(5,3),
                       // segmentPositions : List[Point] = {}
                     ) {
  def changeDir(d : Direction) : SnakeActor = copy(headFacing = if (d != headFacing.opposite) d else headFacing)
}
