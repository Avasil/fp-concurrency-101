package tanks

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.html
import shared.models.GameState
import tanks.animation.CanvasImage
import tanks.communication.ServerCommunication
import tanks.game.GameLoop.gameLoop

object ClientApp extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = {
    val backgroundCanvas = dom.document.getElementById("tanks-bg").asInstanceOf[html.Canvas]
    val tanksCanvas      = dom.document.getElementById("tanks").asInstanceOf[html.Canvas]
    val ctx              = tanksCanvas.getContext("2d").asInstanceOf[Ctx2D]
    val bgCtx            = backgroundCanvas.getContext("2d").asInstanceOf[Ctx2D]

    for {
//      gameState <- ServerCommunication.initialize()
      gameState <- Task(Observable(GameState.mapOne) ++ Observable.never[GameState])
//          Observable(GameState.mapTwo).delayExecution(200.millis) ++
//          Observable(GameState.mapThree).delayExecution(200.millis) ++
//          Observable.never[GameState])
      bgImage <- loadImage(bgCtx)
      image   <- loadImage(ctx)
      canvasImage = new CanvasImage(ctx, bgCtx, image, bgImage)
      _ <- canvasImage.drawBackground()
      _ <- gameLoop(canvasImage, gameState)
      _ <- Task(println("completed"))
    } yield ExitCode.Success
  }
}
