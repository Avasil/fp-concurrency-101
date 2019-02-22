package tanks.game

import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.reactive.Observable
import shared.models.GameObject.{Bullet, Tank}
import shared.models._
import tanks.game.logic.{CollisionLogic, Destroyed, TankMovement}

import scala.concurrent.duration._

object GameLoop {

  def apply(
    playerInputs: ConcurrentQueue[Task, MovementCommand],
    initialState: GameState
  ): GameLoop =
    new GameLoop(playerInputs, initialState)
}

final class GameLoop private (
  playerInputs: ConcurrentQueue[Task, MovementCommand],
  initialState: GameState
) extends CollisionLogic with TankMovement {

  // TODO: 7. GameLoop
  // There are many possible implementation but my recommendation is this:
  // - Observable starts with initial state
  // - Then it continuously polls Queue with inputs from players
  // - We don't need very low latency and evolving the state with each incoming move probably won't end well so
  //   try buffering moves in specific intervals (I used 150 millis) and choose one move (e.g. the latest one) per player
  // - Finally, you have to apply the changes to the last game state (logic is already implemented),
  //   send delta state downstream and keep the new state in the `Observable`. `Observable.scan0` might be handy.

  // Observable.groupBy will create sub streams so you will have to look for a way to merge them and keep in mind
  // that depending on how you merge the substreams you might need a buffer per sub-stream AND a
  // buffer for elements from merged stream

  // Useful Links:
  // https://monix.io/api/3.0/monix/reactive/Observable.html
  // https://monix.io/docs/3x/reactive/observable.html (incomplete, feedback and contributions welcome!)
  // https://github.com/monix/monix/blob/master/monix-reactive/shared/src/main/scala/monix/reactive/Observable.scala

  def gameStateObservable: Observable[GameState] = ???
// You can apply the game logic in this way:
// ========================================================================
//  val (updatedPlayers: Map[Int, Tank], updatedBullets: Map[Int, Bullet]) =
//    moveTank(players, bullets, environment, commands)
//
//  val (destroyed, deltaState) =
//    resolveCollisions(GameState(updatedPlayers, updatedBullets, environment))
//
//  (Destroyed.update(GameState.mergeDelta(gameState, deltaState), destroyed), deltaState)
//  ========================================================================
//  `Destroyed.update(a, b)` will resolve changes to damaged objects and the deltaState is what we have to send downstream.

}
