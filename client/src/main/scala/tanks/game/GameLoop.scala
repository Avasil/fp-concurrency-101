package tanks.game

import cats.implicits._
import monix.catnap.Semaphore
import monix.eval.Task
import monix.reactive.Observable
import shared.models.GameObject.Water
import shared.models.{AnimatedObject, GameObject, GameState}
import tanks.canvas.CanvasImage

object GameLoop {

  def gameLoop(canvas: CanvasImage, obs: Observable[GameState]): Task[Unit] = {
    for {
      sem <- Semaphore[Task](1L)
      _ <- obs
        .scan(GameState.empty)(GameState.mergeDelta)
        .mapEval { gameState =>
          val (animated, static, water) = split(gameState)
          val animateTask =
            for {
              _ <- canvas.drawEnvironment(static)
              _ <- sem.withPermit(canvas.animateWater(water).loopForever).start
              _ <- canvas.drawMovement(animated)
              _ <- canvas.drawExplosions(animated).start
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
