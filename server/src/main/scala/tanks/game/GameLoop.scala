package tanks.game

import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.reactive.Observable
import shared.models.GameObject.{Bullet, Tank}
import shared.models._
import tanks.game.logic.{CollisionLogic, Destroyed, TankMovement}

import scala.concurrent.duration._

object GameLoop {

  def create(playerInputs: ConcurrentQueue[Task, MovementCommand], initialState: GameState): GameLoop =
    new GameLoop(playerInputs, initialState)
}

final class GameLoop private (playerInputs: ConcurrentQueue[Task, MovementCommand], initialState: GameState)
    extends CollisionLogic with TankMovement {

  def gameStateObservable: Observable[GameState] =
    Observable
      .repeatEvalF(playerInputs.poll)
      .groupBy(_.id)
      .mergeMap(_.throttleLast(150.millis))
      .bufferTimed(150.millis)
      .scan0((initialState, initialState)) {
        case ((gameState @ GameState(players, bullets, environment), _), commands) =>
          val (updatedPlayers: Map[Int, Tank], updatedBullets: Map[Int, Bullet]) =
            resolveTankMovement(players, bullets, commands)

          val (destroyed, deltaState) =
            resolveCollisions(GameState(updatedPlayers, updatedBullets, environment))

//          println(deltaState)
          // remove from new state but send last path to the client
          (Destroyed.update(GameState.combine(gameState, deltaState), destroyed), deltaState)
      }
      .map { case (_, delta) => delta }

}