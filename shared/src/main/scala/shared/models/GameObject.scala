package shared.models

sealed trait GameObject {
  def position: (Double, Double)
}

sealed trait MovingObject extends GameObject {
  def direction: Direction

  def id: Int
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
  final case class Tank(id: Int, team: Team, position: (Double, Double), direction: Direction) extends MovingObject

  // if bullet hits tank there is a small explosion
  final case class Bullet(id: Int, position: (Double, Double), direction: Direction) extends MovingObject

  final case class Water(position: (Double, Double)) extends GameObject

  final case class Ground(position: (Double, Double)) extends GameObject

  final case class Grass(position: (Double, Double)) extends GameObject

  final case class BrickWall(health: Int, position: (Double, Double)) extends GameObject

  final case class SteelWall(health: Int, position: (Double, Double)) extends GameObject
}