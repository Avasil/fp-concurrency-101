package tanks

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}

object ServerApp extends TaskApp {

  override def run(args: List[String]): Task[ExitCode] = {
    val logger = Logger.create
    for {
      _ <- logger.log("Hello server")
    } yield ExitCode.Success
  }
}
