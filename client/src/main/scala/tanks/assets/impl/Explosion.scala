package tanks.assets.impl

import tanks.assets.{standardHeight, standardWidth, AnimatedAsset, ResourceLocation}

object Explosion

object ExplosionAsset extends AnimatedAsset[Explosion.type] {
  private val loc = ResourceLocation(256, 128, standardWidth, standardHeight)

  override def dynamicOffset(a: Explosion.type): Stream[ResourceLocation] = {
    Stream(loc,
           loc.copy(offsetX = 272),
           loc.copy(offsetX = 288),
           loc,
           ResourceLocation(352, 0, standardWidth, standardHeight)) #::: dynamicOffset(a)
  }

  override def resourceLocation(a: Explosion.type): ResourceLocation =
    loc
}
