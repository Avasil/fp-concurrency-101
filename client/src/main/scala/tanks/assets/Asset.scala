package tanks.assets

import shared.models.GameObject._
import shared.models.{AnimatedObject, Direction, GameObject}
import tanks.assets.impl._

trait Asset[A] {
  def resourceLocation(a: A): ResourceLocation
}

trait AnimatedAsset[A] extends Asset[A] {
  def dynamicOffset(a: A): Stream[ResourceLocation]
}

object Asset {

  implicit val explosionAsset: AnimatedAsset[Explosion.type] = ExplosionAsset

  implicit val tankAsset: AnimatedAsset[Tank] = TankAsset

  implicit val bulletAsset: AnimatedAsset[Bullet] = BulletAsset

  implicit val waterAsset: AnimatedAsset[Water] = WaterAsset

  implicit val movingObjectAsset: AnimatedAsset[AnimatedObject] = new AnimatedAsset[AnimatedObject] {
    override def dynamicOffset(a: AnimatedObject): Stream[ResourceLocation] = a match {
      case t: Tank => tankAsset.dynamicOffset(t)
      case b: Bullet => bulletAsset.dynamicOffset(b)
    }

    override def resourceLocation(a: AnimatedObject): ResourceLocation = a match {
      case t: Tank => tankAsset.resourceLocation(t)
      case b: Bullet => bulletAsset.resourceLocation(b)
    }
  }

  implicit val gameObjectAsset: Asset[GameObject] = {
    case o: AnimatedObject => movingObjectAsset.resourceLocation(o)
    case o: Water => waterAsset.resourceLocation(o)
    case Grass(_) => ResourceLocation(256, 64, standardWidth, standardHeight)
    case b: BrickWall => brickLocation(b)
    case SteelWall(_) => ResourceLocation(256, 16, standardWidth, standardHeight)
  }

  private def brickLocation(brickWall: BrickWall): ResourceLocation = {
    val offsetX =
      brickWall.hitDirection.fold(256) {
        case Direction.UP => 288
        case Direction.DOWN => 320
        case Direction.LEFT => 272
        case Direction.RIGHT => 304
      }
    ResourceLocation(offsetX, 0, standardWidth, standardHeight)
  }
}
