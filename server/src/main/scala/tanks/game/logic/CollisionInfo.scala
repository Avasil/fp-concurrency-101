package tanks.game.logic
import shared.models.{AnimatedObject, GameObject}
import shared.models.GameObject.Bullet

final case class CollisionInfo(bullet: Bullet, distance: Double, point: (Int, Int), obj: GameObject)

object CollisionInfo {

  def dist(o1: AnimatedObject, collisionPoint: (Int, Int)): Double = {
    val (x1, y1) = o1.prevPosition
    val (x2, y2) = collisionPoint

    Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
  }
}
