package tanks.communication

import io.circe.parser.decode
import io.circe.syntax._
import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.WebSocket
import shared.models.ComMessage.msgEncoder
import shared.models.{GameState, WelcomeMessage}

object ServerCommunication {
  def init(serverUrl: String = "ws://localhost:8080/ws"): Task[Observable[GameState]] = {
    for {
      // TODO: Do I need ConcurrentQueue? Probably not. Still a good exercise. :)
      queue <- ConcurrentQueue.unbounded[Task, GameState]()
      socket = new WebSocket(serverUrl)
      server = new ServerCommunication(socket, queue)
      _ <- server.setup()
    } yield server.gameState
  }
}

final class ServerCommunication private(socket: dom.WebSocket, queue: ConcurrentQueue[Task, GameState]) {
  def setup(): Task[Unit] =
    for {
      _ <- waitForSocket()
      _ <- sendWelcomeMessage()
      _ <- sendKeys()
      _ <- receiveNewState()
    } yield ()

  private def waitForSocket(): Task[Unit] = Task.async { cb =>
    socket.onopen = { _ =>
      cb.onSuccess(())
    }
  }

  private def sendWelcomeMessage(): Task[Unit] = {
    Task(socket.send(WelcomeMessage("afafa").asJson.noSpaces))
  }

  private def sendKeys(): Task[Unit] = Task {
    dom.window.addEventListener("keyup", (e: dom.KeyboardEvent) => {
      socket.send(e.key)
    }, useCapture = false)
  }

  private def receiveNewState(): Task[Unit] = Task.deferAction { implicit s =>
    Task {
      socket.onmessage = (e: dom.MessageEvent) => {
        decode[GameState](e.data.toString).fold(
          err => println(s"couldnt decode ${err.getMessage}"),
          msg => queue.offer(msg).runToFuture
        )
      }
    }
  }

  private def gameState: Observable[GameState] =
    Observable.repeatEvalF(queue.poll)
}