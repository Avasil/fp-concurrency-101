package tanks.assets.impl
import shared.models.Direction
import shared.models.GameObject.BrickWall
import tanks.assets.{standardHeight, standardWidth, Asset, ResourceLocation}

object BrickWallAsset extends Asset[BrickWall] {
  private def brickLocation(brickWall: BrickWall): ResourceLocation = {
    val offsetX =
      brickWall.hitDirection.fold(256) {
        case Direction.UP    => 320
        case Direction.DOWN  => 288
        case Direction.LEFT  => 304
        case Direction.RIGHT => 272
      }
    ResourceLocation(offsetX, 0, standardWidth, standardHeight)
  }
  override def resourceLocation(a: BrickWall): ResourceLocation =
    if (a.hp <= 0) ResourceLocation(352, 0, standardWidth, standardHeight) else brickLocation(a)
}
