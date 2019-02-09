package tanks
package assets

import monix.eval.Task
import org.scalajs.dom.raw.HTMLImageElement

final case class CanvasImage(ctx: Ctx2D, image: HTMLImageElement) {
  def drawMoving[A: MovingAsset](asset: A, posX: Double, posY: Double): Task[Unit] =
    Task {
      val pos = asset.position
      ctx.drawImage(
        image = image,
        offsetX = asset.getAnimatedOffsetX(), // source offset
        offsetY = pos.offsetY,
        width = pos.width, // in source image
        height = pos.height,
        // on the screen
        canvasOffsetX = posX,
        canvasOffsetY = posY,
        canvasImageWidth = pos.width,
        canvasImageHeight = pos.height
      )
    }

  def draw(asset: Asset, posX: Double, posY: Double): Task[Unit] =
    Task {
      val pos = asset.resourceLocation
      ctx.drawImage(
        image = image,
        offsetX = pos.offsetX,
        offsetY = pos.offsetY,
        width = pos.width,
        height = pos.height,
        canvasOffsetX = posX,
        canvasOffsetY = posY,
        canvasImageHeight = pos.height,
        canvasImageWidth = pos.width
      )
    }
}
