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

trait MovingAsset[A] extends AnimatedAsset[A] {
  def stepSize(a: A): Int
}

object Asset {

  implicit val explosionAsset: AnimatedAsset[Explosion.type] = ExplosionAsset

  implicit val tankAsset: MovingAsset[Tank] = TankAsset

  implicit val bulletAsset: MovingAsset[Bullet] = BulletAsset

  implicit val waterAsset: AnimatedAsset[Water] = WaterAsset

  implicit val movingObjectAsset: MovingAsset[AnimatedObject] = new MovingAsset[AnimatedObject] {
    override def dynamicOffset(a: AnimatedObject): Stream[ResourceLocation] = a match {
      case t: Tank   => tankAsset.dynamicOffset(t)
      case b: Bullet => bulletAsset.dynamicOffset(b)
    }

    override def resourceLocation(a: AnimatedObject): ResourceLocation = a match {
      case t: Tank   => tankAsset.resourceLocation(t)
      case b: Bullet => bulletAsset.resourceLocation(b)
    }
    override def stepSize(a: AnimatedObject): Int = a match {
      case t: Tank   => tankAsset.stepSize(t)
      case b: Bullet => bulletAsset.stepSize(b)
    }
  }

  implicit val gameObjectAsset: Asset[GameObject] = {
    case o: AnimatedObject => movingObjectAsset.resourceLocation(o)
    case o: Water          => waterAsset.resourceLocation(o)
    case Grass(_)          => ResourceLocation(256, 64, standardWidth, standardHeight)
    case b: BrickWall      => BrickWallAsset.resourceLocation(b)
    case SteelWall(_)      => ResourceLocation(256, 16, standardWidth, standardHeight)
  }
}
