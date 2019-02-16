package tanks

import cats.effect.ExitCode
import cats.implicits._
import monix.catnap.Semaphore
import monix.eval.{Task, TaskApp}
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.html
import shared.models.GameObject.{Bullet, Water}
import shared.models.{AnimatedObject, GameObject, GameState}
import tanks.animation.CanvasImage
import tanks.assets.{standardHeight, standardWidth, Asset, ResourceLocation}
import tanks.communication.ServerCommunication

import scala.concurrent.duration._

object ClientApp extends TaskApp {

  def split(gameState: GameState): (List[AnimatedObject], List[GameObject], List[Water]) = {
    val (animated, other) =
      (gameState.players.values ++ gameState.bullets.values ++ gameState.environment.values).toList.partitionEither {
        case a: AnimatedObject => Left(a)
        case a: GameObject => Right(a)
      }

    val (water, static) =
      other.partitionEither {
        case w: Water => Left(w)
        case otherwise => Right(otherwise)
      }

    (animated, static, water)
  }

  override def run(args: List[String]): Task[ExitCode] = {
    def gameLoop(canvas: CanvasImage, obs: Observable[GameState]): Task[Unit] =
      Semaphore[Task](0L).flatMap { sem =>
        obs
          .scan(GameState.empty)(GameState.combine)
          .mapEval { gameState =>
            val animateTask =
              for {
                _ <- sem.release
                (animated, static, water) = split(gameState)
                _ <- canvas.drawAll(static)
                _ <- Task
                  .race(canvas.animateWater(water).loopForever, sem.awaitAvailable(2L))
                  .guarantee(sem.acquire)
                  .start
                _ <- canvas
                  .drawMovement(animated)
//                  .timed
//                  .map(a => println(a._1))
                _ <- canvas.drawExplosions(animated).start
              } yield ()
            animateTask
          }
          .completedL
      }

    val backgroundCanvas = dom.document.getElementById("tanks-bg").asInstanceOf[html.Canvas]
    val tanksCanvas = dom.document.getElementById("tanks").asInstanceOf[html.Canvas]
    val ctx = tanksCanvas.getContext("2d").asInstanceOf[Ctx2D]
    val bgCtx = backgroundCanvas.getContext("2d").asInstanceOf[Ctx2D]

    for {
      gameState <- ServerCommunication.initialize()
//      gameState <- Task(
//        Observable(GameState.mapOne) ++
//          Observable(GameState.mapTwo).delayExecution(200.millis) ++
//          Observable(GameState.mapThree).delayExecution(200.millis) ++
//          Observable.never[GameState])
      bgImage <- loadImage(bgCtx)
      image <- loadImage(ctx)
      canvasImage = new CanvasImage(ctx, bgCtx, image, bgImage)
      _ <- canvasImage.drawBackground(ResourceLocation(352, 0, standardWidth, standardHeight))
      _ <- gameLoop(canvasImage, gameState)
    } yield ExitCode.Success
  }
}
