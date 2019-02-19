package tanks.communication

import cats.syntax.flatMap._
import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.WebSocket
import shared.models.{GameState, Movement, MovementCommand, WelcomeMessage}

import scala.collection.mutable
import scala.concurrent.duration._

object ServerCommunication {

  def initialize(serverUrl: String = "ws://localhost:8080/ws"): Task[Observable[GameState]] = {
    for {
      queue <- ConcurrentQueue.unbounded[Task, GameState]()
      socket = new WebSocket(serverUrl)
      server = new ServerCommunication(socket, queue)
      _ <- server.setup()
    } yield server.gameState
  }
}

final class ServerCommunication private (socket: dom.WebSocket, queue: ConcurrentQueue[Task, GameState]) {

  def setup(): Task[Unit] =
    for {
      _ <- waitForSocket()
      _ <- sendWelcomeMessage()
      _ <- addKeyListeners()
      _ <- receiveNewState()
      _ <- sendMovementCommand().start
    } yield ()

  private def gameState: Observable[GameState] =
    Observable.repeatEvalF(queue.poll)

  private def waitForSocket(): Task[Unit] =
    Task.async { cb =>
      socket.onopen = { _ =>
        cb.onSuccess(())
      }
    }

  private def sendWelcomeMessage(): Task[Unit] = {
    Task(socket.send(WelcomeMessage("Hi!").encode.noSpaces))
  }

  private def receiveNewState(): Task[Unit] = Task.deferAction { implicit s =>
    Task {
      socket.onmessage = (e: dom.MessageEvent) => {
        GameState
          .decode(e.data.toString)
          .fold(
            err => println(s"Could not decode ${err.getMessage}"),
            msg => {
              queue.offer(msg).runToFuture
            }
          )
      }
    }
  }

  private def sendMovementCommand(): Task[Unit] =
    Observable
      .intervalAtFixedRate(20.millis)
      .mapEval(_ =>
        checkUserInput.flatMap(_.fold(Task.unit)(movement =>
          Task(println(s"sending $movement")) >> Task(socket.send(movement.encode.toString())))))
      .completedL

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
