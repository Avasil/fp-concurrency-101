package shared.models
import io.circe.{Decoder, Encoder}
import io.circe.generic.extras.auto._
import io.circe.generic.extras.Configuration

sealed trait ComMessage

final case class WelcomeMessage(id: String) extends ComMessage

object ComMessage {
  implicit val genDevConfig: Configuration = Configuration.default

  implicit def msgDecoder[T <: ComMessage]: Decoder[T] = implicitly[Decoder[T]]
  implicit def msgEncoder[T <: ComMessage]: Encoder[T] = implicitly[Encoder[T]]
}