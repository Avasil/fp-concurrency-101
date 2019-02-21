package tanks.animation

import cats.implicits._
import monix.eval.{Coeval, Task}
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLImageElement
import shared.models.GameObject.{movementCoords, Bullet, Tank, Water}
import shared.models.{AnimatedObject, GameObject}
import tanks.Ctx2D
import tanks.assets.impl.Explosion
import tanks.assets.{AnimatedAsset, Asset, ResourceLocation, standardHeight, standardWidth, _}

import scala.concurrent.duration._

final class CanvasImage(tanksCtx: Ctx2D, bgCtx: Ctx2D, tanksImage: HTMLImageElement, bgImage: HTMLImageElement) {

  def drawBackground(): Task[Unit] = Task {
    bgCtx.fillRect(0, 0, 320, 320)
  }

  def drawEnvironment[A <: GameObject: Asset](assets: List[A]): Coeval[Unit] = {
    Coeval
      .traverse(assets) { asset =>
        val (x, y) = asset.destination
        bgCtx.fillRect(x, y, standardWidth, standardHeight)
        draw(asset.resourceLocation, x, y, bgCtx, bgImage)
      }
      .void
  }

  def draw(
    pos: ResourceLocation,
    posX: Double,
    posY: Double,
    ctx: Ctx2D = tanksCtx,
    image: HTMLImageElement = tanksImage
  ): Coeval[Unit] =
    Coeval {
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

  def drawAnimated[A: AnimatedAsset](
    asset: A,
    forms: Int,
    posX: Double,
    posY: Double,
    interval: FiniteDuration
  ): Task[Unit] = {
    asset.getAnimatedOffsetX.take(forms).toList.traverse_ { resourceLocation =>
      Task.from(draw(resourceLocation, posX, posY, bgCtx, bgImage)) >> Task.sleep(interval)
    }
  }

  def animateWater(assets: List[Water]): Task[Unit] =
    Task
      .wanderUnordered(assets)(water => drawAnimated(water, 2, water.destination._1, water.destination._2, 250.millis))
      .void

  def drawMovement[A <: AnimatedObject: AnimatedAsset](assets: List[A]): Task[Unit] = {
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
//              if (movementSteps.size > 1)
//                println(s"clear $prevX, $prevY, draw $x, $y")
              tanksCtx.clearRect(prevX, prevY, standardWidth, standardHeight)
              draw(resourceLocation, x, y).runTry()
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
}
