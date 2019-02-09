package tanks

package object assets {
  implicit class AssetSyntax[A](a: A)(implicit A: Asset[A]) {
    def position: ResourceLocation = A.resourceLocation
  }

  implicit class MovingAssetSyntax[A](a: A)(implicit A: MovingAsset[A]) extends AssetSyntax[A](a) {
    def getAnimatedOffsetX(): Double = A.dynamicOffset()
  }
}
