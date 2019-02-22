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

  // TODO: 2. Animations
  // This function draws explosions if the bullets collide with each other or enemy tank.
  // I've left coordinates already calculated but try drawing them concurrently.
  // To draw animation use `drawAnimated(Explosion, 5, x, y, 100.millis)`
  def drawExplosions(assets: List[AnimatedObject]): Task[Unit] = {
    val explosions: List[(Int, Int)] =
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

    ???
  }

  // TODO: 2. Animations
  // Well, it's not really efficient way to do this on top of JS but try to solve it with `Task`:
  // - draw all animation frames with specified intervals between invocations
  // - So: frame1 -> delay -> frame2 -> delay ... -> frameN -> delay
  // - you can draw like this: Task(draw(resourceLocation, posX, posY, bgCtx, bgImage))
  private def drawAnimated[A: AnimatedAsset](
    asset: A,
    forms: Int,
    posX: Double,
    posY: Double,
    interval: FiniteDuration
  ): Task[Unit] = {
    val frames: List[ResourceLocation] = asset.getAnimatedOffsetX.take(forms).toList

    ???
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

  // TODO: 1. Creating a Task from Callback
  // We need to wait until the image is loaded before doing anything with it.
  // JS API provides just a callback but that's enough. :)
  // Look around in Task API for functions such as `Task.create`, `Task.async`, `Task.cancelable` etc. and choose what
  // you think is appropriate!
  def loadImage(ctx: Ctx2D, src: String): Task[HTMLImageElement] = {
    val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    image.src = src
    image.onload = { event =>
      ???
    }
    ???
  }
}
