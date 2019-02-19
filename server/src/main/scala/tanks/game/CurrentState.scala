package tanks.game
import cats.effect.concurrent.Ref
import monix.eval.Task
import shared.models.GameState

trait CurrentState {
  def get: Task[GameState]
}

object CurrentState {

  def apply(ref: Ref[Task, GameState]): CurrentState = new CurrentState {
    override def get: Task[GameState] = ref.get
  }
}
