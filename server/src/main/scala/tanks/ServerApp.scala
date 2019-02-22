package tanks

import cats.effect.ExitCode
import cats.effect.concurrent.Ref
import monix.catnap.ConcurrentQueue
import monix.eval.{Task, TaskApp}
import shared.models._
import tanks.comm.WebSocket
import tanks.game.{BotService, CurrentState, GameLoop, GameStatus}

// TODO: 6. Server setup
// - Create playerInputs and gameStateQueue which are `ConcurrentQueue` from `monix.catnap`.
// - Pass them to WebSocket `new WebSocket(playersInputs, gameStateQueue, gameStatus, logger)(scheduler)`
// - Pass playerInputs to GameLoop
// - GameLoop should also add elements to `gameStateQueue` after calculating the state after resolving player movements
object ServerApp extends TaskApp {

  override def run(args: List[String]): Task[ExitCode] = {
    val logger       = Logger.create
    val initialState = GameState.mapOne

    for {
      _ <- logger.log("Hello server")

      // TODO: 8. Bots
      // If we want the bots to have any signs of intelligence we should pass the latest state there
      // and allow them to send their own movements.
      // - Create gameStateRef that you can pass there
      // - Update gameStateRef whenever the state changed
      // - If you want to support restarting games without restarting the server you will have
      //   to reset the Ref whenever `gameStateObservable` finishes

//      currentState = CurrentState(gameStateRef)
//      botService   = new BotService(playersInputs, currentState)
//      _ <- botService.runBotsLoop(List.range(1, 11))

      // TODO: 6. Server setup
      // Uncomment once 7. is done
//      _ <- ws.stream.compile.drain
    } yield ExitCode.Success
  }
}
