package tanks.comm
import monix.eval.Task
import org.scalajs.dom
import shared.models.{CommMesage, GameState}

final class WebSocket private (socket: dom.WebSocket) {

  def open(): Task[Unit] =
    Task.async { cb =>
      socket.onopen = _ => cb.onSuccess(())
    }

  def send(msg: CommMesage): Task[Unit] = Task.eval {
    socket.send(msg.encode.toString())
  }

  def doOnMessage[A](f: GameState => Task[A]): Task[Unit] = Task.deferAction { implicit s =>
    Task {
      socket.onmessage = (e: dom.MessageEvent) => {
        GameState
          .decode(e.data.toString)
          .fold(
            err => println(s"Could not decode ${err.getMessage}"),
            msg => {
              f(msg).runToFuture
            }
          )
      }
    }
  }
}

object WebSocket {

  def apply(url: String): Task[WebSocket] = Task.eval {
    new WebSocket(new dom.WebSocket(url))
  }
}
