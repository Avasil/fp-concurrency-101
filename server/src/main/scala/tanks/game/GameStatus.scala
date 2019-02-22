package tanks.game

import monix.eval.Task

// TODO: 10. GameStatus
// This class should contain information whether the game is running or not.
// It could be used to restart the game if the client established new connection
// and run some parts (like bots and the game loop) only when it's active.

// waitForStart should semantically block until the game is started.
// waitForEnd should semantically block until the game is done.
// runWhenActive should run `task` in argument only when the game is running.
// One of possible implementations could be in terms of waitForStart, waitForEnd and Task.race
trait GameStatus {
  def start: Task[Unit]
  def stop: Task[Unit]
  def waitForStart: Task[Unit]
  def waitForEnd: Task[Unit]
  def runWhenActive(task: Task[Unit]): Task[Unit]
}

object GameStatus {

  def apply(): Task[GameStatus] = Task {
    new GameStatus {
      override def start: Task[Unit]        = Task.unit
      override def stop: Task[Unit]         = Task.unit
      override def waitForStart: Task[Unit] = ???
      override def waitForEnd: Task[Unit]   = ???

      override def runWhenActive(task: Task[Unit]): Task[Unit] = ???
    }
  }
}
