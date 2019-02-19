import monix.catnap.ConcurrentQueue
import monix.eval.Task
import shared.models.{GameState, MovementCommand}

package object tanks {

  type PlayersMovesQueue = ConcurrentQueue[Task, MovementCommand]

  type GameStateQueue = ConcurrentQueue[Task, GameState]
}
