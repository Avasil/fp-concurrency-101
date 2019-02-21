package shared.models

import io.circe
import io.circe.Json
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.syntax._

sealed trait CommMesage {
  final def encode: Json = CommMesage.encode(this)
}

final case class WelcomeMessage(id: String) extends CommMesage

final case class MovementCommand(id: Int, movement: Movement) extends CommMesage

sealed trait Movement

object Movement {
  case object Fire extends Movement

  case object Up extends Movement

  case object Down extends Movement

  case object Right extends Movement

  case object Left extends Movement

  def fromDirection(direction: Direction): Movement = direction match {
    case Direction.UP    => Up
    case Direction.DOWN  => Down
    case Direction.LEFT  => Left
    case Direction.RIGHT => Right
  }

  def fromNumber(n: Int): Movement = n match {
    case 0 => Up
    case 1 => Down
    case 2 => Right
    case 3 => Left
    case _ => Fire
  }
}

object CommMesage {
  implicit val genDevConfig: Configuration = Configuration.default

  private def encode(msg: CommMesage): Json =
    msg.asJson

  def decode(s: String): Either[circe.Error, CommMesage] =
    io.circe.parser.decode[CommMesage](s)
}
