package tanks

package communication

import cats.effect._
import cats.effect.concurrent.MVar
import cats.implicits._
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
import shared.models.{CommMesage, MovementCommand, WelcomeMessage}
import tanks.game.GameStatus

final class WebSocket(
  clientInputs: PlayersMovesQueue,
  gameStateQueue: GameStateQueue,
  game: GameStatus,
  logger: Logger
)(implicit s: Scheduler)
    extends Http4sDsl[Task] {

  private def routes: HttpRoutes[Task] = HttpRoutes.of[Task] {
    case GET -> Root / "ws" =>
      val toClient: Stream[Task, WebSocketFrame] =
        Stream
          .repeatEval(gameStateQueue.poll)
          .map(s => Text(s.encode.toString()))

      val fromClient: Pipe[Task, WebSocketFrame, Unit] = _.evalMap {
        case Close(_) =>
          logger.log("Closing connection!") >> game.stop >> gameStateQueue.clear
        case Text(t, _) =>
          CommMesage
            .decode(t)
            .fold(
              err => logger.log(s"Couldn't parse message: ${err.getMessage}"),
              msg => {
                for {
                  _ <- logger.log(s"Received msg $msg")
                  _ <- msg match {
                    case WelcomeMessage(_)  => game.start
                    case m: MovementCommand => clientInputs.offer(m)
                  }
                } yield ()
              }
            )
        case f =>
          logger.log(s"Unknown message: $f")
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
