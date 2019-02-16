package tanks.game.logic
import shared.models.GameState

final case class Destroyed(tanks: List[Int], bullets: List[Int], environment: List[(Int, Int)])

object Destroyed {

  def update(g: GameState, d: Destroyed): GameState = {
    GameState(g.players -- d.tanks, g.bullets -- d.bullets, g.environment -- d.environment)
  }
}
