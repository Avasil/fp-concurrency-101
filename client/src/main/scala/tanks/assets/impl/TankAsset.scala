package tanks.assets.impl

import shared.models.GameObject.Tank
import shared.models.{Direction, Team}
import tanks.assets.{standardHeight, standardWidth, AnimatedAsset, MovingAsset, ResourceLocation}

object TankAsset extends MovingAsset[Tank] {
  private def offsetX(direction: Direction, team: Team): Int = {
    val x1 = direction match {
      case Direction.UP    => 0
      case Direction.DOWN  => 64
      case Direction.LEFT  => 32
      case Direction.RIGHT => 96
    }

    val x2 = team match {
      case Team.Silver | Team.Purple => 128
      case Team.Yellow | Team.Green  => 0
    }
    x1 + x2
  }

  private def offsetY(team: Team): Int =
    team match {
      case Team.Green | Team.Purple  => 128
      case Team.Yellow | Team.Silver => 0
    }

  override def dynamicOffset(a: Tank): Stream[ResourceLocation] = {
    val loc  = resourceLocation(a)
    val loc2 = loc.copy(offsetX = loc.offsetX + 16)

    def loop: Stream[ResourceLocation] = Stream(loc, loc2) #::: loop

    loop
  }

  override def resourceLocation(a: Tank): ResourceLocation =
    ResourceLocation(offsetX(a.direction, a.team), offsetY(a.team), standardWidth, standardHeight)

  override def stepSize(a: Tank): Int = 2
}
