package tanks

import cats.effect.ExitCode
import monix.catnap.ConcurrentQueue
import monix.eval.{Task, TaskApp}
import monix.reactive.Consumer
import shared.models.{GameState, MovementCommand}
import tanks.communication.WebSocket
import tanks.game.GameLoop

// TODO: make sure to send terminate msg in bracket in case the server dies
object ServerApp extends TaskApp {

  override def options: Task.Options =
    Task.defaultOptions.enableLocalContextPropagation

  override def run(args: List[String]): Task[ExitCode] = {
    val logger = Logger.create
    for {
      _ <- logger.log("Hello server")
      clientInputs <- ConcurrentQueue.unbounded[Task, MovementCommand]()
      updatedStates <- ConcurrentQueue.unbounded[Task, GameState]()
      // TODO: work on communication, start with preset map and then just send updates
      ws = new WebSocket(clientInputs, updatedStates, logger)(scheduler)
      _ <- updatedStates.offer(GameState.mapOne)
      _ <- GameLoop
        .start(clientInputs)
        .gameState()
        .consumeWith(Consumer.foreachEval(updatedStates.offer))
        .start
      _ <- ws.stream.compile.drain
    } yield ExitCode.Success
  }
}
