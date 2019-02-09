import monix.catnap.ConcurrentQueue
import monix.eval.Task
import shared.models.{ComMessage, GameState}

package object tanks {

  type InputQueue = ConcurrentQueue[Task, ComMessage]

  type OutputQueue = ConcurrentQueue[Task, GameState]
}
