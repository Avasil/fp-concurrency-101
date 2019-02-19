package tanks

import cats.effect.ExitCode
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.flatMap._
import monix.catnap.ConcurrentQueue
import monix.eval.{Task, TaskApp}
import shared.models._
import tanks.communication.WebSocket
import tanks.game.{BotService, CurrentState, GameLoop}

// TODO: make sure to send terminate msg in bracket in case the server dies
object ServerApp extends TaskApp {

  override def run(args: List[String]): Task[ExitCode] = {
    val logger       = Logger.create
    val initialState = GameState.mapOne

    for {
      _              <- logger.log("Hello server")
      playersInputs  <- ConcurrentQueue.unbounded[Task, MovementCommand]()
      gameStateQueue <- ConcurrentQueue.unbounded[Task, GameState]()
      _              <- gameStateQueue.offer(initialState)
      gameStarted    <- Deferred[Task, Unit]
      gameStateRef   <- Ref[Task].of[GameState](initialState)
      ws = new WebSocket(playersInputs, gameStateQueue, gameStarted, logger)(scheduler)
      gameStateObservable = GameLoop
        .create(playersInputs, initialState)
        .gameStateObservable
      _ <- (gameStarted.get >> gameStateObservable
        .doOnNext(latestState => gameStateRef.modify(lastState => (GameState.combine(lastState, latestState), ())))
        .mapEval(gameStateQueue.offer)
        .completedL).start
      currentState = CurrentState(gameStateRef)
      botService   = new BotService(playersInputs, currentState)
      _ <- botService.runBotsLoop(List(1, 2, 3, 4, 5))
      _ <- ws.stream.compile.drain
    } yield ExitCode.Success
  }
}
