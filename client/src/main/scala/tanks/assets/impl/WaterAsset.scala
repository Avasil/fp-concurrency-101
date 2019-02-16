package tanks.assets.impl

import shared.models.GameObject.Water
import tanks.assets.{AnimatedAsset, ResourceLocation, standardHeight, standardWidth}

object WaterAsset extends AnimatedAsset[Water] {
  private val loc = ResourceLocation(256, 48, standardWidth, standardHeight)

  override def dynamicOffset(a: Water): Stream[ResourceLocation] =
    Stream(loc, loc.copy(offsetX = 272))

  override def resourceLocation(a: Water): ResourceLocation =
    loc

}
