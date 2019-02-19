package tanks.game

import cats.effect.concurrent.Deferred
import cats.implicits._
import monix.catnap.Semaphore
import monix.eval.Task
import monix.reactive.Observable
import shared.models.GameObject.Water
import shared.models.{AnimatedObject, GameObject, GameState}
import tanks.animation.CanvasImage

import scala.concurrent.duration._

object GameLoop {

  def gameLoop(canvas: CanvasImage, obs: Observable[GameState]): Task[Unit] = {
    for {
      stopSignal <- Deferred[Task, Unit]
      sem        <- Semaphore[Task](1L)
      _ <- obs
        .scan(GameState.empty)(GameState.combine)
        .mapEval { gameState =>
          val (animated, static, water) = split(gameState)
          val animateTask =
            for {
              _ <- canvas.drawAll(static)
              _ <- Task.race(sem.withPermit(canvas.animateWater(water).loopForever), stopSignal.get).start
              _ <- canvas.drawMovement(animated)
              _ <- canvas.drawExplosions(animated).start
            } yield ()
          animateTask
        }
        .completedL
        .guarantee(stopSignal.complete(()).attempt.void)
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
