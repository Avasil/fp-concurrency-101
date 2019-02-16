package shared.models

import io.circe
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.auto._
import io.circe.syntax._
import io.circe.{Json, KeyDecoder, KeyEncoder}
import shared.models.GameObject._

// TODO: do not include tanks in the state, let stuff overlap tanks
final case class GameState(
  players: Map[Int, Tank],
  bullets: Map[Int, Bullet],
  environment: Map[(Int, Int), EnvObject]) {
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

  private val edges = (x: Int, y: Int) => (x == 0 || x == 304) || (y == 0 || y == 304)
  private val edges2 = (x: Int, y: Int) => (x == 16 || x == 288) || (y == 16 || y == 288)

  val empty = GameState(Map.empty, Map.empty, Map.empty)

  // TODO: removing players and bullets, e.g. tanks and bullets are by id
  def combine(g1: GameState, g2: GameState): GameState = {
    GameState(g2.players, g2.bullets, g1.environment ++ g2.environment)
  }

  val staticAssets: Map[(Int, Int), EnvObject] =
    (for {
      x <- 0 to (304, 16)
      y <- 0 to (304, 16)
    } yield {
      if (edges(x, y)) Some(SteelWall((x, y)))
      else if (edges2(x, y)) Some(BrickWall((x, y), None))
      else if (y == 128) Some(Water((x, y)))
      else None
    }).collect { case Some(cos) => cos.position -> cos }.toMap

  val dynamicAssets: Map[Int, Tank] = List(
    0 -> Tank(0, Team.Green, (80, 80), (80, 80), Direction.DOWN),
    1 -> Tank(1, Team.Yellow, (80, 160), (80, 160), Direction.UP)
  ).toMap

  def mapOne: GameState = {
    GameState(dynamicAssets, Map.empty, staticAssets)
  }

  def mapTwo: GameState = {
    val tanks: Map[Int, Tank] = List(
      0 -> Tank(0, Team.Green, (80, 96), (80, 80), Direction.DOWN),
      1 -> Tank(1, Team.Yellow, (80, 176), (80, 160), Direction.UP)
    ).toMap

    GameState(tanks, Map.empty, staticAssets)
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

    GameState(tanks, bullets, staticAssets)
  }
}
