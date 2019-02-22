package tanks.game

import cats.implicits._
import monix.eval.Task
import monix.reactive.Observable
import shared.models.GameObject.Water
import shared.models.{AnimatedObject, GameObject, GameState}
import tanks.canvas.CanvasImage

object GameLoop {

  // If you've completed all "1" todos you should be able to run the game
  def gameLoop(canvas: CanvasImage, obs: Observable[GameState]): Task[Unit] = {
    for {
      _ <- obs
        .scan(GameState.empty)(GameState.mergeDelta)
        .mapEval { gameState =>
          val (animated, static, water) = split(gameState)
          val animateTask =
            for {
              _ <- canvas.drawEnvironment(static)
              // TODO: 3. Water animation.
              // We would like to animate water in a loop but we want it to be happening
              // in the background. The water can't be destroyed so ideally we could start
              // infinite loop once and make sure there is only one such a task running.
              // Once you do it, check out the game!
//              _ <- ??? canvas.animateWater(water) ???
              _ <- canvas.drawMovement(animated)
              // TODO: 2. Animations
              // Check instructions in `CanvasImage`.
              // Result should be bullet exploding if you run testGame. :)
//              _ <- canvas.drawExplosions(animated)
            } yield ()
          animateTask
        }
        .completedL
    } yield ()
  }

  private def split(gameState: GameState): (List[AnimatedObject], List[GameObject], List[Water]) = {
    val (animated, other) =
      (gameState.players.values ++ gameState.bullets.values ++ gameState.environment.values).toList.partitionEither {
        case a: AnimatedObject => Left(a)
        case a: GameObject     => Right(a)
      }

    val (water, static) =
      other.partitionEither {
        case w: Water  => Left(w)
        case otherwise => Right(otherwise)
      }

    (animated, static, water)
  }
}
