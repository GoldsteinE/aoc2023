case class Point(x: Int, y: Int, z: Int) {
  def up: Point = Point(x, y, z + 1)
  def down: Point = Point(x, y, z - 1)
  def moveTo(other: Point): Point =
    val dx = (other.x - x).sign
    val dy = (other.y - y).sign
    val dz = (other.z - z).sign
    Point(x + dx, y + dy, z + dz)
  def stepsTo(other: Point): LazyList[Point] =
    this #:: (
      if this == other
      then LazyList.empty
      else this.moveTo(other).stepsTo(other)
    )
}

case class Brick(start: Point, end: Point) {
  def cubes: LazyList[Point] =
    // Cubes are iterated from the bottom to make falling easier.
    val (start_, end_) = if start.z <= end.z then (start, end) else (end, start)
    start_.stepsTo(end_)
}

object Brick {
  def parse(line: String): Brick =
    val Array(rawStart, rawEnd) = line.split("~")
    val Array(sx, sy, sz) = rawStart.split(",").map(_.toInt)
    val Array(ex, ey, ez) = rawEnd.split(",").map(_.toInt)
    Brick(Point(sx, sy, sz), Point(ex, ey, ez))
}

def bricksToGrid(bricks: Array[Brick]): Map[Point, Int] = 
  (for (brick, idx) <- bricks.view.zipWithIndex
       cube         <- brick.cubes
  yield (cube, idx)).toMap

def fall(bricks: Array[Brick]): (Set[Int], Map[Point, Int]) =
  var grid = bricksToGrid(bricks)
  var fallen = Set[Int]()
  var moved = true
  while moved do
    moved = false
    for idx <- bricks.indices do
      val brick = bricks(idx)
      if brick.start.z != 1 && brick.end.z != 1 then
        val supported = brick.cubes
          .map(p => grid.get(p.down))
          .find(_.filter(_ != idx).nonEmpty).nonEmpty
        if !supported then
          moved = true
          fallen = fallen.incl(idx)
          for cube <- brick.cubes do
            grid = grid - cube + ((cube.down, idx))
          bricks(idx) = Brick(brick.start.down, brick.end.down)
  (fallen, grid)

@main def main(args: String*) =
  val bricks = io.Source.stdin.getLines.map(Brick.parse).toArray
  val (fallen, grid) = fall(bricks)

  var safe = bricks.indices.toSet
  for (brick, idx) <- bricks.view.zipWithIndex do
    val supportedBy = brick.cubes.map(p => grid.get(p.down)).flatMap(identity).toSet - idx
    if supportedBy.size == 1 then
      safe = safe - supportedBy.head

  if args(0) == "1" then
    println(safe.size)
  else
    val totalDestruction =
      bricks.view.zipWithIndex
        .filter((_, idx) => !safe.contains(idx))
        .map((brick, _) => fall(bricks.filter(_ != brick))(0).size)
        .sum
    println(totalDestruction)
