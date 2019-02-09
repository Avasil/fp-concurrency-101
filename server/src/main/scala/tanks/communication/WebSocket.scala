package tanks.communication

import cats.effect._
import io.circe.parser.decode
import io.circe.syntax._
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
import shared.models.ComMessage
import tanks.{InputQueue, Logger, OutputQueue}

final class WebSocket(clientInputs: InputQueue,
                      sendQueue: OutputQueue,
                      logger: Logger)(implicit s: Scheduler)
  extends Http4sDsl[Task] {

  private def routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case GET -> Root / "ws" =>
      // TODO: to co dostaje wrzucam na jedno, ale odsylac powinienem indywidualnie (mogą być różne gry)
      val toClient: Stream[Task, WebSocketFrame] =
        Stream.repeatEval(sendQueue.poll).map(s => Text(s.asJson.toString()))

      val fromClient: Pipe[Task, WebSocketFrame, Unit] = _.evalMap {
        case Text(t, _) =>
          decode[ComMessage](t).fold(
            err => logger.log(s"Couldn't parse message: ${err.getMessage}"),
            msg => clientInputs.offer(msg)
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