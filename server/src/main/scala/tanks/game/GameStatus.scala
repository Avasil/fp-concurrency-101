package tanks.game

import cats.effect.concurrent.MVar
import cats.syntax.functor._
import cats.syntax.flatMap._
import monix.eval.Task

trait GameStatus {
  def start: Task[Unit]
  def stop: Task[Unit]
  def waitForStart: Task[Unit]
  def waitForEnd: Task[Unit]

  def runWhenActive(task: Task[Unit]): Task[Unit]
}

object GameStatus {

  def apply(): Task[GameStatus] = {
    for {
      startMVar <- MVar[Task].empty[Unit]
      endMVar   <- MVar[Task].empty[Unit]
    } yield
      new GameStatus {
        override def start: Task[Unit]        = startMVar.tryPut(()) >> endMVar.tryTake.void
        override def stop: Task[Unit]         = endMVar.tryPut(()) >> startMVar.tryTake.void
        override def waitForStart: Task[Unit] = startMVar.read
        override def waitForEnd: Task[Unit]   = endMVar.read

        override def runWhenActive(task: Task[Unit]): Task[Unit] = {
          waitForStart >> Task.race(waitForEnd, task) >> runWhenActive(task)
        }
      }
  }
}
