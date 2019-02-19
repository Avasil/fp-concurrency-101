package tanks

import monix.eval.Task

trait Logger {
  def log(msg: String): Task[Unit]
}

object Logger {
  def create: Logger = (msg: String) => Task.delay(println(msg))
}
