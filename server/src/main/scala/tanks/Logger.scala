package tanks

import monix.eval.{Task, TaskLocal}

trait Logger {
  def log(msg: String): Task[Unit]
}

trait TracedLogger extends Logger {
  def setTraceId(id: Option[String]): Task[Unit]
}

object Logger {
  def create: Logger = (msg: String) => Task.delay(println(msg))
}

object TracedLogger {
  // TODO: fix
  def create: Task[TracedLogger] = TaskLocal[Option[String]](None).map { taskLocal =>
    new TracedLogger {
      override def setTraceId(id: Option[String]): Task[Unit] =
        taskLocal.write(id)

      override def log(msg: String): Task[Unit] =
        taskLocal
          .read
          .flatMap {
            case Some(traceId) => Task.delay(println(s"$traceId: $msg"))
            case None => Task.delay(println(msg))
          }
    }
  }
}