package tanks

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import org.scalajs.dom
import org.scalajs.dom.html

object ClientApp extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] = {
    val backgroundCanvas = dom.document.getElementById("tanks-bg").asInstanceOf[html.Canvas]
    val tanksCanvas      = dom.document.getElementById("tanks").asInstanceOf[html.Canvas]
    val ctx              = tanksCanvas.getContext("2d").asInstanceOf[Ctx2D]
    val bgCtx            = backgroundCanvas.getContext("2d").asInstanceOf[Ctx2D]

    for {
      bgImage <- loadImage(bgCtx)
      image   <- loadImage(ctx)
      _       <- Task(println("completed"))
    } yield ExitCode.Success
  }
}
