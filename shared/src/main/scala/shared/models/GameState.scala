package shared.models

import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.{Decoder, Encoder}
import shared.models.GameObject._

final case class GameState(state: List[GameObject])

object GameState {

  private val edges = (x: Int, y: Int) => (x == 0 || x == 304) || (y == 0 || y == 304)
  private val edges2 = (x: Int, y: Int) => (x == 16 || x == 288) || (y == 16 || y == 288)

  def mapOne: GameState = {
    val staticAssets: List[GameObject] =
      (for {
        x <- 0 to(304, 16)
        y <- 0 to(304, 16)
      } yield {
        if (edges(x, y)) SteelWall(4, (x, y))
        else if (edges2(x, y)) BrickWall(4, (x, y))
        else if (y == 128) Water((x, y))
        else Ground((x, y))
      }).toList

    def dynamicAssets: List[GameObject] = List(
      Tank(0, (80, 80), Direction.DOWN),
      Tank(1, (80, 160), Direction.UP)
    )

    GameState(staticAssets ++ dynamicAssets)
  }

  implicit val genDevConfig: Configuration = Configuration.default

  implicit def msgDecoder[T <: GameState]: Decoder[T] = implicitly[Decoder[T]]

  implicit def msgEncoder[T <: GameState]: Encoder[T] = implicitly[Encoder[T]]
}