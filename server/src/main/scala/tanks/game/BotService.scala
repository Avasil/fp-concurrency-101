package tanks.game
import cats.syntax.functor._
import monix.eval.Task
import shared.models.{Movement, MovementCommand}
import tanks.PlayersMovesQueue

import scala.concurrent.duration._
import scala.util.Random

final class BotService(playersInputs: PlayersMovesQueue, currentState: CurrentState) {

  def runBotsLoop(ids: List[Int]): Task[Unit] =
    Task.traverse(ids)(runBot(_).delayExecution(500.millis).loopForever.start).void

  def runBot(id: Int): Task[Unit] =
    for {
      gameState <- currentState.get
      _ <- if (!gameState.players.contains(id)) Task.unit
      else {
        for {
          move <- randomMove
          _    <- playersInputs.offer(MovementCommand(id, move))
        } yield ()
      }
    } yield ()

  private def randomMove: Task[Movement] = Task {
    Movement.fromNumber(Random.nextInt(5))
  }

}
