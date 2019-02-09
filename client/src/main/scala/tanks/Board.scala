package tanks

import cats.syntax.functor._
import monix.eval.Task
import shared.models.GameState
import tanks.assets._

case class AssetLocation(x: Double, y: Double, asset: Asset)

object Board {

  def draw(gameState: GameState): Task[Unit] = {

  }

  def drawStatic(assets: List[AssetLocation], canvas: CanvasImage): Task[Unit] = {
    val draw: List[Task[Unit]] =
      assets.map {
        case AssetLocation(x, y, asset) =>
          canvas.draw(asset, x, y)
      }

    Task.sequence(draw).void
  }

  def drawDynamic(assets: List[AssetLocation], canvas: CanvasImage): Task[Unit] = {
    val draw: List[Task[Unit]] =
      assets.map {
        case AssetLocation(x, y, asset: MovingAsset) =>
          canvas.drawMoving(asset, x, y)
        case AssetLocation(x, y, asset) =>
          canvas.draw(asset, x, y)
      }

    Task.sequence(draw).void
  }

  private val edges = (x: Int, y: Int) => (x == 0 || x == 304) || (y == 0 || y == 304)
  private val edges2 = (x: Int, y: Int) => (x == 16 || x == 288) || (y == 16 || y == 288)

  val staticAssets: List[AssetLocation] =
    (for {
      x <- 0 to (304, 16)
      y <- 0 to (304, 16)
    } yield {
      if (edges(x, y)) AssetLocation(x, y, Terrain.SteelWall())
      else if (edges2(x, y)) AssetLocation(x, y, Terrain.BrickWall())
      else if (y == 128) AssetLocation(x, y, Terrain.Water)
      else AssetLocation(x, y, Terrain.Ground)
    }).toList

  val tankYellow = Tanks.yellow.UP
  val tankGreen = Tanks.green.DOWN

  def dynamicAssets(l: Long): List[AssetLocation] = List(
    AssetLocation(80, 80, tankGreen),
    AssetLocation(80, 160, tankYellow)
  )

}
