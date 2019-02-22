package tanks

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.html
import shared.models.GameState
import tanks.canvas.CanvasImage
import tanks.canvas.CanvasImage.Ctx2D
import tanks.comm.ServerCommunication
import tanks.game.GameLoop.gameLoop

import scala.concurrent.duration._

// Useful Links:
// https://monix.io/api/3.0/monix/reactive/Observable.html
// https://monix.io/docs/3x/reactive/observable.html (incomplete, feedback and contributions welcome!)
// https://github.com/monix/monix/blob/master/monix-reactive/shared/src/main/scala/monix/reactive/Observable.scala

// https://github.com/monix/monix/blob/master/monix-eval/shared/src/main/scala/monix/eval/Task.scala
// https://monix.io/docs/3x/eval/task.html
// https://monix.io/api/3.0/monix/eval/Task.html

object ClientApp extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = {
    val backgroundCanvas = dom.document.getElementById("tanks-bg").asInstanceOf[html.Canvas]
    val tanksCanvas      = dom.document.getElementById("tanks").asInstanceOf[html.Canvas]
    val ctx              = tanksCanvas.getContext("2d").asInstanceOf[Ctx2D]
    val bgCtx            = backgroundCanvas.getContext("2d").asInstanceOf[Ctx2D]

    def testGame: Observable[GameState] =
      Observable.fromIterable(GameState.testGame).delayOnNext(150.millis) ++ testGame

    for {
      _ <- Task.eval(println("Hello, client!"))
      // TODO: Uncomment once Server is working. :)
//      gameState <- ServerCommunication.initialize()
      gameState = testGame
      bgImage <- CanvasImage.loadImage(bgCtx, "images/sprites.png")
      image   <- CanvasImage.loadImage(ctx, "images/sprites-transparent.png")
      canvasImage = new CanvasImage(ctx, bgCtx, image, bgImage)
      _ <- canvasImage.drawBackground()
      _ <- gameLoop(canvasImage, gameState)
      _ <- Task(println("completed"))
    } yield ExitCode.Success
  }
}
