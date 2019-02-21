package tanks

package object assets {
  implicit class AssetSyntax[A](a: A)(implicit A: Asset[A]) {
    def resourceLocation: ResourceLocation = A.resourceLocation(a)
  }

  implicit class AnimatedAssetSyntax[A](a: A)(implicit A: AnimatedAsset[A]) {
    def getAnimatedOffsetX: Stream[ResourceLocation] = A.dynamicOffset(a)
  }

  implicit class MovingAssetSyntax[A](a: A)(implicit A: MovingAsset[A]) {
    def stepSize: Int = A.stepSize(a)
  }

  val standardWidth  = 16
  val standardHeight = 16
}
