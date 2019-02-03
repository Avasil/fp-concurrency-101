package tanks

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}

object Main extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] =
    for {
      _ <- Task(println("Welcome"))
    } yield ExitCode.Success
}
