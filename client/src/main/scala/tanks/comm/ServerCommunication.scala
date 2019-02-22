package tanks.comm

import cats.syntax.flatMap._
import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.reactive.Observable
import org.scalajs.dom
import shared.models.{GameState, Movement, MovementCommand, WelcomeMessage}

import scala.collection.mutable
import scala.concurrent.duration._

object ServerCommunication {

  def initialize(serverUrl: String = "ws://localhost:8080/ws"): Task[Observable[GameState]] = {
    for {
      queue  <- ConcurrentQueue.unbounded[Task, GameState]()
      socket <- WebSocket(serverUrl)
      server = new ServerCommunication(socket, queue)
      _ <- server.setup()
    } yield server.gameState
  }
}

final class ServerCommunication private (socket: WebSocket, queue: ConcurrentQueue[Task, GameState]) {

  def setup(): Task[Unit] =
    for {
      _ <- socket.open()
      _ <- sendWelcomeMessage()
      _ <- addKeyListeners()
      _ <- addNewStateToQueue()
      // TODO 5. Sending Movements in a loop
      // We would like to send it in a loop but we also want to return from the function
      // so it all happens "in the background".
      _ <- sendMovementLoop()
    } yield ()

  private def gameState: Observable[GameState] =
    Observable.repeatEvalF(queue.poll)

  private def sendWelcomeMessage(): Task[Unit] = {
    socket.send(WelcomeMessage("Hi!"))
  }

  private def addNewStateToQueue(): Task[Unit] =
    socket.doOnMessage(queue.offer)

  // TODO 5. Sending Movements in a loop
  // Sometimes it's much more convenient to use Streaming-based abstraction,
  // rather than Task.
  // Use `Observable` to write a function that will check userInputs every 20 milliseconds
  // and send it through the socket.
  // `checkUserInput` will give you movement to send and you can use `socket.send(movement)` to send it.
  private def sendMovementLoop(): Task[Unit] = ???

  private val keysDown: mutable.HashSet[Movement] =
    mutable.HashSet[Movement]()

  private def checkUserInput: Task[Option[MovementCommand]] = Task {
    var x = 0
    var y = 0

    if (keysDown.contains(Movement.Left)) x -= 1
    if (keysDown.contains(Movement.Right)) x += 1
    if (keysDown.contains(Movement.Up)) y -= 1
    if (keysDown.contains(Movement.Down)) y += 1

    val movement =
      if (keysDown.contains(Movement.Fire)) Some(Movement.Fire)
      else if (x == -1) Some(Movement.Left)
      else if (x == 1) Some(Movement.Right)
      else if (y == -1) Some(Movement.Up)
      else if (y == 1) Some(Movement.Down)
      else None

    keysDown.clear()
    movement.map(MovementCommand(0, _))
  }

  private def addKeyListeners(): Task[Unit] = Task {
    dom.window.addEventListener("keydown", (e: dom.KeyboardEvent) => {
      movementKey(e.key).foreach(keysDown += _)
    }, false)

    dom.window.addEventListener("keyup", (e: dom.KeyboardEvent) => {
      movementKey(e.key).foreach(keysDown -= _)
    }, false)

  }

  private def movementKey(key: String): Option[Movement] =
    key.toLowerCase() match {
      case "w" => Some(Movement.Up)
      case "s" => Some(Movement.Down)
      case "d" => Some(Movement.Right)
      case "a" => Some(Movement.Left)
      case "e" => Some(Movement.Fire)
      case _   => None
    }
}
