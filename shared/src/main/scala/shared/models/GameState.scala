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
  implicit val genDevConfig: Configuration = Configuration.default

  implicit val tupleKeyEncoder: KeyEncoder[(Int, Int)] = _.asJson.toString()
  implicit val tupleKeyDecoder: KeyDecoder[(Int, Int)] = str => io.circe.parser.decode[(Int, Int)](str).toOption

  private def encode(msg: GameState): Json =
    msg.asJson

  def decode(s: String): Either[circe.Error, GameState] =
    io.circe.parser.decode[GameState](s)

  private val edges      = (x: Int, y: Int) => (x == 0 || x == 304) || (y == 0 || y == 304)
  private val edges2     = (x: Int, y: Int) => (x == 16 || x == 288) || (y == 16 || y == 288)
  private val brickWalls = (x: Int, y: Int) => Set(128, 144, 160, 176).contains(y)

  val empty = GameState(Map.empty, Map.empty, Map.empty)

  def combine(g1: GameState, g2: GameState): GameState = {
    val env = (g1.environment ++ g2.environment).filter {
      case (_, BrickWall(_, _, 0)) => false
      case _                       => true
    }
    GameState(g2.players, g2.bullets, env)
  }

  val environment: Map[(Int, Int), EnvObject] =
    (for {
      x <- 0 to (304, 16)
      y <- 0 to (304, 16)
    } yield {
      if (edges(x, y)) Some(SteelWall((x, y)))
      else if (edges2(x, y)) Some(BrickWall((x, y), None))
      else if ((y == 128 || y == 176) && x != 144 && x != 160) Some(Water((x, y)))
      else if (brickWalls(x, y)) Some(BrickWall((x, y), None))
      else None
    }).collect { case Some(cos) => cos.position -> cos }.toMap

  def mapOne: GameState = {
    val tanks: Map[Int, Tank] = List(
      0 -> Tank(0, Team.Green, (80, 240), (80, 240), Direction.UP),
      1 -> Tank(1, Team.Yellow, (112, 80), (112, 80), Direction.DOWN),
      2 -> Tank(2, Team.Yellow, (128, 80), (128, 80), Direction.DOWN),
      3 -> Tank(3, Team.Yellow, (160, 80), (160, 80), Direction.DOWN),
      4 -> Tank(4, Team.Yellow, (192, 80), (192, 80), Direction.DOWN),
      5 -> Tank(5, Team.Yellow, (224, 80), (224, 80), Direction.DOWN)
    ).toMap

    GameState(tanks, Map.empty, environment)
  }

  def mapTwo: GameState = {
    val tanks: Map[Int, Tank] = List(
      0 -> Tank(0, Team.Green, (80, 96), (80, 80), Direction.DOWN),
      1 -> Tank(1, Team.Yellow, (80, 176), (80, 160), Direction.UP)
    ).toMap

    GameState(tanks, Map.empty, environment)
  }

  def mapThree: GameState = {
    val tanks: Map[Int, Tank] = Map(
      0 -> Tank(0, Team.Green, (80, 96), (80, 96), Direction.DOWN),
      1 -> Tank(1, Team.Yellow, (80, 176), (80, 176), Direction.UP)
    )

    val bullets = Map(
      2 -> Bullet(2, (80, 96), (80, 160), Direction.UP),
      3 -> Bullet(3, (80, 176), (80, 96), Direction.DOWN)
    )

    GameState(tanks, bullets, environment)
  }
}
