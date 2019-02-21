package tanks.assets.impl

import shared.models.Direction
import shared.models.GameObject.Bullet
import tanks.assets.{standardHeight, standardWidth, MovingAsset, ResourceLocation}

object BulletAsset extends MovingAsset[Bullet] {

  private def offsetX(direction: Direction): Int =
    direction match {
      case Direction.UP    => 320
      case Direction.DOWN  => 336
      case Direction.LEFT  => 328
      case Direction.RIGHT => 344
    }

  override def dynamicOffset(a: Bullet): Stream[ResourceLocation] =
    Stream.continually(resourceLocation(a))

  override def resourceLocation(a: Bullet): ResourceLocation =
    ResourceLocation(offsetX(a.direction), 96, standardWidth / 2, standardHeight)

  override def stepSize(a: Bullet): Int = 4
}
