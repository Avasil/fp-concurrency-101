package tanks.game

import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable
import shared.models.GameObject.{BrickWall, Bullet, Tank, Water}
import shared.models._
import tanks.game.logic.{CollisionInfo, CollisionLogic, Destroyed}

import scala.annotation.tailrec
import scala.concurrent.duration._

object GameLoop {

  def start(playerInputs: ConcurrentQueue[Task, MovementCommand]): GameLoop =
    new GameLoop(playerInputs)
}

final class GameLoop private (playerInputs: ConcurrentQueue[Task, MovementCommand]) extends CollisionLogic {

  // TODO: handle multiple players
  def gameState(): Observable[GameState] =
    Observable
      .repeatEvalF(playerInputs.poll)
      .bufferTimed(100.millis)
      .map(_.lastOption)
      .scan0((GameState.mapOne, GameState.mapOne)) {
        case ((g @ GameState(players, bullets, environment), _), Some(MovementCommand(id, movement))) =>
          players.get(id).fold((g, GameState.empty)) { tank =>
            val (x, y) = tank.position
            val updatedTank = movement match {
              case Movement.Fire =>
                tank.copy(prevPosition = tank.position)
              case Movement.Up =>
                tank.copy(direction = Direction.UP, prevPosition = tank.position, position = (x, y - 16))
              case Movement.Down =>
                tank.copy(direction = Direction.DOWN, prevPosition = tank.position, position = (x, y + 16))
              case Movement.Right =>
                tank.copy(direction = Direction.RIGHT, prevPosition = tank.position, position = (x + 16, y))
              case Movement.Left =>
                tank.copy(direction = Direction.LEFT, prevPosition = tank.position, position = (x - 16, y))
            }

            val newBullets: Map[Int, Bullet] =
              if (movement.isInstanceOf[Movement.Fire.type]) {
                val bullet = createBullet(updatedTank)
                Map(bullet.id -> bullet)
              } else {
                Map.empty
              }

            val newPlayers = players ++ Map(id -> updatedTank)

            val (destroyed, deltaState) =
              collisions(GameState(newPlayers, bullets ++ newBullets, environment))

            println(deltaState)

            // remove from new state but send last path to the client
            (Destroyed.update(GameState.combine(g, deltaState), destroyed), deltaState)
          }
        case ((gameState, _), None) =>
          val (destroyed, deltaState) = collisions(gameState)

          println(deltaState)
          (Destroyed.update(GameState.combine(gameState, deltaState), destroyed), deltaState)
      }
      .map { case (_, delta) => delta }

  private def createBullet(shooter: Tank): Bullet = {
    val (x, y) = shooter.position
    val position = shooter.direction match {
      case Direction.UP => (x, y - 16)
      case Direction.DOWN => (x, y + 16)
      case Direction.LEFT => (x - 16, y)
      case Direction.RIGHT => (x + 16, y)
    }

    Bullet(newId(), position, position, shooter.direction)
  }

  private val lastBulletId = AtomicInt(0)
  private[this] def newId(): Int = {
    lastBulletId.getAndIncrement()
  }
}
