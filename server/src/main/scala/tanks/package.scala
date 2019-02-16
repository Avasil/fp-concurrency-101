import monix.catnap.ConcurrentQueue
import monix.eval.Task
import shared.models.{GameState, MovementCommand}

package object tanks {

  type InputQueue = ConcurrentQueue[Task, MovementCommand]

  type OutputQueue = ConcurrentQueue[Task, GameState]
}
