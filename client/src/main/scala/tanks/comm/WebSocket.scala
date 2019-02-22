package tanks.comm
import monix.eval.Task
import org.scalajs.dom
import shared.models.{CommMesage, GameState}

final class WebSocket private (socket: dom.WebSocket) {

  // TODO: 1. Creating a Task from Callback
  // We need to wait until the socket is opened
  // JS API provides just a callback but that's enough. :)
  // Look around in Task API for functions such as `Task.create`, `Task.async`, `Task.cancelable` etc. and choose what
  // you think is appropriate!
  def open(): Task[Unit] = {
    socket.onopen = event => ???
    ???
  }

  def send(msg: CommMesage): Task[Unit] = Task.eval {
    socket.send(msg.encode.toString())
  }

  // TODO 4. Impure interactions
  // Sometimes we need to interact with impure API and there's no way around it but
  // we still want to hide this behind pure interface.

  // `doOnMessage` accepts f: GameState => Task[A] that has to be run every time
  // we receive valid message through the socket but the socket callback doesn't know
  // how to run the `Task` so we have to do it in a callback ourselves.
  // Still, we should encapsulate it with Task and it would be cool if we didn't have to ask for
  // Scheduler in the function signature. Fortunately, there is a function just for that!

  // Check out `Task.deferAction`
  def doOnMessage[A](f: GameState => Task[A]): Task[Unit] = {
    socket.onmessage = (e: dom.MessageEvent) => {
      GameState
        .decode(e.data.toString)
        .fold(
          err => println(s"Could not decode ${err.getMessage}"),
          msg => {
            f(msg) // run f(msg) somehow
          }
        )
    }
    ???
  }
}

object WebSocket {

  def apply(url: String): Task[WebSocket] = Task.eval {
    new WebSocket(new dom.WebSocket(url))
  }
}
