package tanks

import cats.effect.ExitCode
import monix.catnap.ConcurrentQueue
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable
import shared.models.{ComMessage, GameState}
import tanks.communication.WebSocket

// TODO: make sure to send terminate msg in bracket in case the server dies
object ServerApp extends TaskApp {

  override def options: Task.Options =
    Task.defaultOptions.enableLocalContextPropagation

  override def run(args: List[String]): Task[ExitCode] =
    for {
      _ <- Task(println("Hello server"))
      logger <- TracedLogger.create
      clientInputs <- ConcurrentQueue.unbounded[Task, ComMessage]()
      updatedStates <- ConcurrentQueue.bounded[Task, GameState](2)
      _ <- updatedStates.offer(GameState.mapOne)
      ws = new WebSocket(clientInputs, updatedStates, logger)(scheduler)
      _ <- gameLoop(clientInputs, updatedStates, logger).start
      _ <- ws.stream.compile.drain
    } yield ExitCode.Success

  def gameLoop(playerInputs: InputQueue, updatedStates: OutputQueue, logger: Logger): Task[Unit] = {
    Observable
      .repeatEvalF(playerInputs.poll)
      .mapEval(_ => updatedStates.offer(GameState.mapOne))
      .completedL
  }
}
