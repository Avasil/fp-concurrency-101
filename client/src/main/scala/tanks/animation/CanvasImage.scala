package tanks.animation

import cats.implicits._
import monix.eval.Task
import org.scalajs.dom.raw.HTMLImageElement
import shared.models.GameObject.{movementCoords, Water}
import shared.models.{AnimatedObject, GameObject}
import tanks.Ctx2D
import tanks.assets.impl.Explosion
import tanks.assets.{AnimatedAsset, Asset, ResourceLocation, standardHeight, standardWidth, _}

import scala.concurrent.duration._

final class CanvasImage(ctx: Ctx2D, backgroundContext: Ctx2D, image: HTMLImageElement, bgImage: HTMLImageElement) {

  def drawBackground(): Task[Unit] = Task {
    backgroundContext.fillRect(0, 0, 320, 320)
  }

  def draw(pos: ResourceLocation, posX: Double, posY: Double): Task[Unit] =
    Task {
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

  def drawAll[A <: GameObject: Asset](assets: List[A]): Task[Unit] = {
    Task
      .traverse(assets) { asset =>
        val (x, y) = asset.position
        draw(asset.resourceLocation, x, y)
      }
      .void
  }

  def drawAnimated[A: AnimatedAsset](
    asset: A,
    forms: Int,
    posX: Double,
    posY: Double,
    interval: FiniteDuration
  ): Task[Unit] = {
    asset.getAnimatedOffsetX.take(forms).toList.traverse_ { resourceLocation =>
      draw(resourceLocation, posX, posY) >> Task.sleep(interval)
    }
  }

  // TODO: Move outside
  def animateWater(assets: List[Water]): Task[Unit] = {
    Task.wanderUnordered(assets)(water => drawAnimated(water, 2, water.position._1, water.position._2, 250.millis)).void
  }

  // TODO: Move outside
  def drawMovement[A <: AnimatedObject: AnimatedAsset](assets: List[A]): Task[Unit] = {

    def drawMovementSteps(asset: A): Task[Unit] = {
      val (destX, destY) = asset.position
      val (fromX, fromY) = asset.prevPosition

      movementCoords(fromX, fromY, destX, destY).toStream
        .zip(asset.getAnimatedOffsetX)
        .foldLeft(Task(asset.prevPosition)) {
          case (prevPositionTask, ((posX, posY), resourceLocation)) =>
            for {
              (prevX, prevY) <- prevPositionTask
              _              <- Task(ctx.clearRect(prevX, prevY, standardWidth, standardHeight))
              _              <- draw(resourceLocation, posX, posY)
            } yield (posX, posY)
        }
        .void
    }

//    def fillSteps(maxLength: Int, tasks: List[Task[Unit]]): List[Task[Unit]] = {
//      val sizeDiff: Int = maxLength - tasks.length
//
//      if (sizeDiff > 0) {
//        val lastTask = tasks.lastOption.getOrElse(Task.unit)
//        tasks ++ List.fill(sizeDiff)(lastTask)
//      } else tasks
//    }
//
//    val movementAnimationSteps: List[Task[Unit]] =
//      assets.map(drawMovementSteps)

//    val sameSizeTasks: List[Observable[Task[Unit]]] =
//      movementAnimationSteps
//        .map(_.length)
//        .maximumOption
//        .fold(List.empty[Observable[Task[Unit]]]) { maxLength: Int =>
//          movementAnimationSteps.map { tasks =>
//            Observable.fromIterable(fillSteps(maxLength, tasks))
//          }
//        }

    Task.traverse(assets)(drawMovementSteps).void

//    Observable
//      .zipList(sameSizeTasks: _*)
////      .doOnNext(_ => clearCanvas)
////      .mapEval(tasks => Task.sequence(tasks))
//      .foldLeftL(Task.unit) {
//        case (acc, tasks) =>
//          val animateTask = Task.sequence(tasks)
//          acc >> animateTask >> clearCanvas
//      }
//      .flatten
  }

  // TODO: explosion should start during contact
  // TODO: can be improved with map of positions
  // TODO: track bullets trajectory
  // TODO: Move outside
  def drawExplosions(assets: List[GameObject]): Task[Unit] = {
    assets
      .groupByNel(_.position)
      // TODO: bullets vs bullets and bullets vs tanks diff animation
      .filter { case (_, values) => values.size > 1 }
      .keySet
      .parTraverse_ {
        case (x, y) =>
          drawAnimated(Explosion, 5, x, y, 100.millis)
      }
  }
}
