package shared.models

import io.circe
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.syntax._
import io.circe.{Json, KeyDecoder, KeyEncoder}
import shared.models.GameObject._

final case class GameState(
  players: Map[Int, Tank],
  bullets: Map[Int, Bullet],
  environment: Map[(Int, Int), EnvObject]
) {
  def encode: Json = GameState.encode(this)
}

object GameState {
  val empty: GameState = GameState(Map.empty, Map.empty, Map.empty)

  def mergeDelta(g1: GameState, g2: GameState): GameState = {
    GameState(g2.players, g2.bullets, g1.environment ++ g2.environment)
  }

  def environment: Map[(Int, Int), EnvObject] = {
    val edges      = (x: Int, y: Int) => (x == 0 || x == 304) || (y == 0 || y == 304)
    val edges2     = (x: Int, y: Int) => (x == 16 || x == 288) || (y == 16 || y == 288)
    val brickWalls = (x: Int, y: Int) => Set(128, 144, 160, 176).contains(y)

    (for {
      x <- 0 to (304, 16)
      y <- 0 to (304, 16)
    } yield {
      if (edges(x, y)) Some(SteelWall((x, y)))
      else if (edges2(x, y)) Some(BrickWall((x, y), None))
      else if ((y == 128 || y == 176) && x != 144 && x != 160) Some(Water((x, y)))
      else if (brickWalls(x, y)) Some(BrickWall((x, y), None))
      else None
    }).collect { case Some(cos) => cos.destination -> cos }.toMap
  }

  def mapOne: GameState = {
    val tanks: Map[Int, Tank] = List(
      0 -> Tank(0, Team.Green, (80, 240), (80, 240), Direction.UP),
      1 -> Tank(1, Team.Silver, (112, 80), (112, 80), Direction.DOWN),
      2 -> Tank(2, Team.Silver, (128, 80), (128, 80), Direction.DOWN),
      3 -> Tank(3, Team.Silver, (160, 80), (160, 80), Direction.DOWN),
      4 -> Tank(4, Team.Silver, (192, 80), (192, 80), Direction.DOWN),
      5 -> Tank(5, Team.Silver, (224, 80), (224, 80), Direction.DOWN)
    ).toMap

    GameState(tanks, Map.empty, environment)
  }

  def testGame: List[GameState] = {
    val tanks1: Map[Int, Tank] = List(
      0 -> Tank(0, Team.Green, (80, 240), (80, 240), Direction.UP),
      5 -> Tank(5, Team.Silver, (80, 80), (80, 80), Direction.DOWN)
    ).toMap

    val tanks2: Map[Int, Tank] = List(
      0 -> Tank(0, Team.Green, (80, 240), (80, 240), Direction.UP),
      5 -> Tank(5, Team.Silver, (80, 80), (80, 80), Direction.DOWN)
    ).toMap

    val bullets2: Map[Int, Bullet] = Map(
      0 -> Bullet(0, Team.Green, (80, 160), (80, 224), Direction.UP),
      1 -> Bullet(1, Team.Silver, (80, 160), (80, 96), Direction.DOWN)
    )

    val tanks3: Map[Int, Tank] = List(
      0 -> Tank(0, Team.Green, (80, 256), (80, 240), Direction.UP),
      5 -> Tank(5, Team.Silver, (80, 64), (80, 80), Direction.DOWN)
    ).toMap

    val tanks4: Map[Int, Tank] = List(
      0 -> Tank(0, Team.Green, (80, 240), (80, 256), Direction.UP),
      5 -> Tank(5, Team.Silver, (80, 80), (80, 64), Direction.DOWN)
    ).toMap

    List(
      GameState(tanks1, Map.empty, environment),
      GameState(tanks2, bullets2, environment),
      GameState(tanks3, Map.empty, environment),
      GameState(tanks4, Map.empty, environment)
    )
  }

  implicit val genDevConfig: Configuration = Configuration.default

  implicit val tupleKeyEncoder: KeyEncoder[(Int, Int)] = _.asJson.toString()
  implicit val tupleKeyDecoder: KeyDecoder[(Int, Int)] = str => io.circe.parser.decode[(Int, Int)](str).toOption

  private def encode(msg: GameState): Json =
    msg.asJson

  def decode(s: String): Either[circe.Error, GameState] =
    io.circe.parser.decode[GameState](s)
}
