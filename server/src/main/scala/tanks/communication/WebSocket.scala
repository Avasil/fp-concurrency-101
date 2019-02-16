package tanks

package communication

import cats.effect._
import fs2._
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket._
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._
import shared.models.{CommMesage, GameState, MovementCommand, WelcomeMessage}

final class WebSocket(clientInputs: InputQueue, sendQueue: OutputQueue, logger: Logger)(implicit s: Scheduler)
    extends Http4sDsl[Task] {

  private def routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case GET -> Root / "ws" =>
      // TODO: to co dostaje wrzucam na jedno, ale odsylac powinienem indywidualnie (mogą być różne gry)
      val toClient: Stream[Task, WebSocketFrame] =
        Stream
          .repeatEval(sendQueue.poll)
          .map(s => Text(s.encode.toString()))

      val fromClient: Pipe[Task, WebSocketFrame, Unit] = _.evalMap {
        case Text(t, _) =>
          CommMesage
            .decode(t)
            .fold(
              err => logger.log(s"Couldn't parse message: ${err.getMessage}"),
              msg => {
                for {
                  _ <- logger.log(s"Received msg $msg")
                  _ <- msg match {
                    case WelcomeMessage(_) => sendQueue.offer(GameState.mapOne)
                    case m: MovementCommand => clientInputs.offer(m)
                  }
                } yield ()
              }
            )
        case f =>
          logger.log(s"Unknown type: $f")
      }

      WebSocketBuilder[Task].build(toClient, fromClient)
  }

  def stream: Stream[Task, ExitCode] =
    BlazeServerBuilder[Task]
      .bindHttp(8080)
      .withWebSockets(enableWebsockets = true)
      .withHttpApp(routes.orNotFound)
      .serve
}
