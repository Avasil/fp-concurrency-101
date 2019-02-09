package tanks.assets

import shared.models.GameObject._
import shared.models.{Direction, GameObject, MovingObject, Team}

trait Asset[A] {
  def resourceLocation: ResourceLocation
}

trait MovingAsset[A] extends Asset[A] {
  def dynamicOffset: Stream[Double]
}

object Asset {

  val width = 16.0
  val height = 16.0

  implicit def tankAsset(tank: Tank): MovingAsset[Tank] = new MovingAsset[Tank] {
    private val offsetX = tank.direction match {
      case Direction.UP => 0.0
      case Direction.DOWN => 64.0
      case Direction.LEFT => 32.0
      case Direction.RIGHT => 96.0
    }

    private val offsetY: Double = tank.team match {
      case Team.Green | Team.Purple => 128.0
      case Team.Yellow | Team.Silver => 0.0
    }

    override def dynamicOffset: Stream[Double] =
      Stream(offsetX, offsetX + 16.0) ++ dynamicOffset

    override def resourceLocation: ResourceLocation = ResourceLocation(offsetX, offsetY, width, height)
  }

  implicit def gameObjectAsset(gobj: GameObject): Asset[GameObject] = new Asset[GameObject] {
    override def resourceLocation: ResourceLocation = gobj match {
      case t: Tank => tankAsset(t).resourceLocation
      case Water(position) => ResourceLocation(16 * 16, 3 * 16.0, width, height)
      case Ground(position) => ResourceLocation(22 * 16, 0.0, width, height)
      case Grass(position) => ResourceLocation(16 * 16, 4 * 16.0, width, height)
      case BrickWall(health, position) => ResourceLocation(256.0, 0.0, width, height)
      case SteelWall(health, position) => ResourceLocation(256.0, 16.0, width, height)
    }
  }
}