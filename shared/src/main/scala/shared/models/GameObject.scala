package shared.models

sealed trait GameObject {
  def position: (Int, Int)
}

sealed trait EnvObject extends GameObject

sealed trait AnimatedObject extends GameObject {
  def direction: Direction

  def prevPosition: (Int, Int)
}

sealed trait Direction

object Direction {

  case object UP extends Direction

  case object DOWN extends Direction

  case object LEFT extends Direction

  case object RIGHT extends Direction

}

sealed trait Team

object Team {

  case object Green extends Team

  case object Yellow extends Team

  case object Silver extends Team

  case object Purple extends Team

}

object GameObject {

  def movementCoords(fromX: Int, fromY: Int, destX: Int, destY: Int, stepSize: Int = 2): List[(Int, Int)] = {
    def step(from: Int, to: Int): Int =
      if (from > to) -stepSize
      else stepSize

    val coords: Seq[(Int, Int)] =
      for {
        x <- fromX to (destX, step(fromX, destX))
        y <- fromY to (destY, step(fromY, destY))
      } yield (x, y)

    coords.toList
  }

  final case class Tank(id: Int, team: Team, position: (Int, Int), prevPosition: (Int, Int), direction: Direction)
      extends AnimatedObject

  final case class Bullet(id: Int, position: (Int, Int), prevPosition: (Int, Int), direction: Direction)
      extends AnimatedObject

  final case class Water(position: (Int, Int)) extends EnvObject

  final case class Grass(position: (Int, Int)) extends EnvObject

  final case class BrickWall(position: (Int, Int), hitDirection: Option[Direction], hp: Int = 2) extends EnvObject

  final case class SteelWall(position: (Int, Int)) extends EnvObject

}
