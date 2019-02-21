package tanks.game
import cats.syntax.functor._
import monix.eval.Task
import shared.models.GameObject.Tank
import shared.models.{Direction, Movement, MovementCommand}
import tanks.PlayersMovesQueue

import scala.concurrent.duration._
import scala.util.Random

final class BotService(playersInputs: PlayersMovesQueue, currentState: CurrentState) {

  def runBotsLoop(ids: List[Int]): Task[Unit] =
    Task.wanderUnordered(ids)(runBot(_).delayExecution(500.millis).loopForever).void

  def runBot(id: Int): Task[Unit] =
    for {
      gameState <- currentState.get
      _ <- if (!gameState.players.contains(id)) Task.unit
      else {
        for {
          move <- randomMove
          _    <- playersInputs.offer(MovementCommand(id, playerInSight(id, gameState.players).getOrElse(move)))
        } yield ()
      }
    } yield ()

  private def playerInSight(id: Int, players: Map[Int, Tank]): Option[Movement] = {
    players.get(id).flatMap { tankBot =>
      val (x, y) = tankBot.destination
      players.filter { case (_, tank) => tank.team != tankBot.team }.collectFirst {
        case (_, tank) if tank.destination._1 == x || tank.destination._2 == y =>
          if (tank.destination._1 > x) {
            if (tankBot.direction == Direction.RIGHT) Movement.Fire
            else Movement.Right
          } else if (tank.destination._1 < x) {
            if (tankBot.direction == Direction.LEFT) Movement.Fire
            else Movement.Left
          } else if (tank.destination._2 > y) {
            if (tankBot.direction == Direction.DOWN) Movement.Fire
            else Movement.Down
          } else {
            if (tankBot.direction == Direction.UP) Movement.Fire
            else Movement.Up
          }
      }
    }

  }

  private def randomMove: Task[Movement] = Task {
    Movement.fromNumber(Random.nextInt(5))
  }

}
