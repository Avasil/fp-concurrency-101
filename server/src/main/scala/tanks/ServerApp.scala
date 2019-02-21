package tanks

import cats.effect.ExitCode
import cats.effect.concurrent.Ref
import monix.catnap.ConcurrentQueue
import monix.eval.{Task, TaskApp}
import shared.models._
import tanks.comm.WebSocket
import tanks.game.{BotService, CurrentState, GameLoop, GameStatus}

object ServerApp extends TaskApp {

  override def run(args: List[String]): Task[ExitCode] = {
    val logger       = Logger.create
    val initialState = GameState.mapOne

    for {
      _              <- logger.log("Hello server")
      playersInputs  <- ConcurrentQueue.unbounded[Task, MovementCommand]()
      gameStateQueue <- ConcurrentQueue.unbounded[Task, GameState]()
      gameStateRef   <- Ref[Task].of[GameState](initialState)
      gameStatus     <- GameStatus()
      ws                  = new WebSocket(playersInputs, gameStateQueue, gameStatus, logger)(scheduler)
      gameStateObservable = GameLoop(gameStatus, playersInputs, initialState).gameStateObservable
      _ <- gameStatus
        .runWhenActive(
          gameStateObservable
            .doOnNext(latestState => gameStateRef.update(lastState => GameState.mergeDelta(lastState, latestState)))
            .mapEval(gameStateQueue.offer)
            .completedL
            .guarantee(gameStateRef.set(initialState))
        )
        .start
      currentState = CurrentState(gameStateRef)
      botService   = new BotService(playersInputs, currentState)
      _ <- gameStatus.runWhenActive(botService.runBotsLoop(List.range(1, 11))).start
      _ <- ws.stream.compile.drain
    } yield ExitCode.Success
  }
}
