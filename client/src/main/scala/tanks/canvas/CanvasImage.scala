package tanks.canvas

import cats.implicits._
import monix.eval.Task
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLImageElement
import shared.models.GameObject.{movementCoords, Bullet, Tank, Water}
import shared.models.{AnimatedObject, GameObject}
import tanks.assets.impl.Explosion
import tanks.assets.{AnimatedAsset, ResourceLocation, standardHeight, standardWidth, _}
import tanks.canvas.CanvasImage.Ctx2D

import scala.concurrent.duration._

final class CanvasImage(tanksCtx: Ctx2D, bgCtx: Ctx2D, tanksImage: HTMLImageElement, bgImage: HTMLImageElement) {

  def drawBackground(): Task[Unit] = Task {
    bgCtx.fillRect(0, 0, 320, 320)
  }

  def drawEnvironment(assets: List[GameObject]): Task[Unit] = {
    Task
      .traverse(assets) { asset =>
        val (x, y) = asset.destination
        bgCtx.fillRect(x, y, standardWidth, standardHeight)
        Task(draw(asset.resourceLocation, x, y, bgCtx, bgImage))
      }
      .void
  }

  def animateWater(assets: List[Water]): Task[Unit] =
    Task
      .wanderUnordered(assets)(water => drawAnimated(water, 2, water.destination._1, water.destination._2, 250.millis))
      .void

  def drawMovement(assets: List[AnimatedObject]): Task[Unit] = {
    def drawMovementSteps(asset: AnimatedObject): Task[Unit] = Task {
      val (destX, destY) = asset.destination
      val (fromX, fromY) = asset.prevPosition

      val movementSteps: List[((Int, Int), ResourceLocation)] =
        movementCoords(fromX, fromY, destX, destY, asset.stepSize)
          .zip(asset.getAnimatedOffsetX)

      if (movementSteps.nonEmpty) {
        def loop(prevPosition: (Int, Int), steps: List[((Int, Int), ResourceLocation)]): Unit = {
          val (prevX, prevY) = prevPosition
          steps match {
            case ((x, y), resourceLocation) :: tail =>
              tanksCtx.clearRect(prevX, prevY, standardWidth, standardHeight)
              draw(resourceLocation, x, y)
              dom.window.requestAnimationFrame(_ => loop((x, y), tail))
            case Nil =>
              if (movementSteps.size > 1)
                tanksCtx.clearRect(prevX, prevY, standardWidth, standardHeight)
          }
        }

        dom.window.requestAnimationFrame(_ => loop(asset.prevPosition, movementSteps))
      } else ()
    }

    Task.traverse(assets)(drawMovementSteps).void
  }

  def drawExplosions(assets: List[AnimatedObject]): Task[Unit] = {
    assets
      .groupBy(_.destination)
      .filter {
        case (_, values) =>
          val diffTeam: Boolean =
            values.map {
              case t: Tank   => t.team
              case b: Bullet => b.team
            }.toSet.size > 1

          values.size > 1 && diffTeam
      }
      .keys
      .toList
      .parTraverse_ {
        case (x, y) =>
          drawAnimated(Explosion, 5, x, y, 100.millis)
      }
  }

  private def drawAnimated[A: AnimatedAsset](
    asset: A,
    forms: Int,
    posX: Double,
    posY: Double,
    interval: FiniteDuration
  ): Task[Unit] = {
    asset.getAnimatedOffsetX.take(forms).toList.traverse_ { resourceLocation =>
      Task(draw(resourceLocation, posX, posY, bgCtx, bgImage)) >> Task.sleep(interval)
    }
  }

  private def draw(
    pos: ResourceLocation,
    posX: Double,
    posY: Double,
    ctx: Ctx2D = tanksCtx,
    image: HTMLImageElement = tanksImage
  ): Unit =
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

object CanvasImage {

  type Ctx2D = dom.CanvasRenderingContext2D

  def loadImage(ctx: Ctx2D, src: String): Task[HTMLImageElement] =
    Task.async { cb =>
      val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
      image.src = src
      image.onload = { _ =>
        cb.onSuccess(image)
      }
    }
}
