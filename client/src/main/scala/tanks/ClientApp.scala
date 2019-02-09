package tanks

import cats.effect.ExitCode
import cats.syntax.flatMap._
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.html
import shared.models.GameState
import tanks.assets.CanvasImage
import tanks.communication.ServerCommunication

import scala.concurrent.duration._

object ClientApp extends TaskApp {

  override def run(args: List[String]): Task[ExitCode] = {
    def gameLoop(generalSprites: CanvasImage, obs: Observable[GameState]): Task[Unit] = {
      obs
        .delayOnNext(100.millis)
        .mapEval(state => Board.drawStatic(Board.staticAssets, generalSprites) >> Board.drawDynamic(Board.dynamicAssets(state), generalSprites))
        .completedL
    }

    val canvas = dom.document.getElementById("tanks").asInstanceOf[html.Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[Ctx2D]

    for {
      gameState <- ServerCommunication.init()
      image <- loadImage(ctx)
      generalSprites = CanvasImage(ctx, image)
      _ <- gameLoop(generalSprites, gameState)
    } yield ExitCode.Success
  }
}