package snake.logic

// you can alter this file!
// case class Point(x: Int, y : Int)
// means
// * x and y are vals (cannot be changed)
// * You can do  Point(2,3) instead of new Point(2,3)
// * == compares x and y values and does not use reference equality
// * toString is implemented as you expect ("Point(2,3)")
//
// This is explained in lecture 4
case class Point(x : Int, y : Int) {
  def +(delta:Point) : Point = Point(x + delta.x, y + delta.y)
  def -(delta:Point) : Point = Point(x - delta.x, y - delta.y)
  def %(denom:Point) : Point = Point(x % denom.x, y % denom.y)
  def wrap(min:Point, max:Point) : Point = {
    var newx : Int = x
    if(x > max.x) newx = min.x
    else if(x < min.x) newx = max.x

    var newy : Int = y
    if(y > max.y) newy = min.y
    else if (y < min.y) newy = max.y

    Point(newx, newy)
  }
}
