package tanks.game.logic
import shared.models.GameObject.{BrickWall, Bullet, Tank, Water}
import shared.models.{Direction, EnvObject, GameObject, GameState}
import tanks.game.logic.CollisionInfo.dist

import scala.annotation.tailrec

trait CollisionLogic {

  def collisions(gameState: GameState): (Destroyed, GameState) = {
    val GameState(tanks, bullets, environment) = collisionWithEnv(gameState)

    val collisionInfo: List[CollisionInfo] = calcCollisionInfo(tanks, bullets, environment)
    val collisionsByBullet: Map[Bullet, CollisionInfo] = findCollisions(collisionInfo)
    val (updatedTanks, updatedBullets, updatedEnv) = updateCollidedObjects(collisionsByBullet)

    val newTanks = tanks ++ updatedTanks.map(tank => tank.id -> tank).toMap
    val newBullets = bullets ++ updatedBullets.map(bullet => bullet.id -> bullet).toMap
    val newEnvironment: Map[(Int, Int), EnvObject] = updatedEnv.map(env => env.position -> env).toMap

    val destroyed =
      Destroyed(updatedTanks.map(_.id), updatedBullets.map(_.id), updatedEnv.filter(_.hp <= 0).map(_.position))

    destroyed -> GameState(newTanks, newBullets, newEnvironment)
  }

  private def collisionWithEnv(gameState: GameState): GameState = {
    val GameState(tanks, bullets, environment) = gameState

    val unstoppableEnv = environment.filter {
      case (_, _: Water) => false
      case _ => true
    }.keySet

    val updatedTanks: Map[Int, Tank] = tanks.mapValues { t: Tank =>
      if (environment.contains(t.position)) t.copy(position = t.prevPosition)
      else t
    }

    val updatedBullets = bullets.mapValues {
      case b @ Bullet(_, (x, y), _, direction) =>
        val (destX, destY) = direction match {
          case Direction.UP => (x, y - 32)
          case Direction.DOWN => (x, y + 32)
          case Direction.LEFT => (x - 32, y)
          case Direction.RIGHT => (x + 32, y)
        }

        val path = GameObject.movementCoords(x, y, destX, destY, stepSize = 16)

        val newDestination: (Int, Int) =
          path
            .takeWhile(!unstoppableEnv.contains(_))
            .lastOption
            .getOrElse((x, y))

        b.copy(position = newDestination, prevPosition = (x, y))
    }

    GameState(updatedTanks, updatedBullets, environment)
  }

  private def calcCollisionInfo(
    tanks: Map[Int, Tank],
    bullets: Map[Int, Bullet],
    environment: Map[(Int, Int), EnvObject]
  ): List[CollisionInfo] = {
    val tanksMovement: List[(Tank, Seq[(Int, Int)])] = tanks.mapValues {
      case t @ Tank(_, _, (posX, posY), (prevX, prevY), _) =>
        t -> GameObject.movementCoords(prevX, prevY, posX, posY)
    }.values.toList

    val brickWalls: Map[(Int, Int), BrickWall] = environment.collect { case (pos, b: BrickWall) => (pos, b) }
    val brickPositions: Set[(Int, Int)] = brickWalls.keySet

    val bulletsMovement: List[(Bullet, Seq[(Int, Int)])] = bullets.mapValues {
      case b @ Bullet(_, (posX, posY), (prevX, prevY), _) =>
        b -> GameObject.movementCoords(prevX, prevY, posX, posY)
    }.values.toList

    bulletsMovement.flatMap {
      case (bullet, bulletPath) =>
        val tankCollisions: List[CollisionInfo] =
          (for { (tank, tankPath) <- tanksMovement } yield
            tankPath
              .zip(bulletPath)
              .collectFirst {
                case ((tankX, tankY), (bulletX, bulletY))
                    if (tankX - 2 to tankX + 2).contains(bulletX) && (tankY - 2 to tankY + 2).contains(bulletY) =>
                  CollisionInfo(bullet, dist(bullet, (tankX, tankY)), (tankX, tankY), tank)
              }).flatten

        val wallCollisions: List[CollisionInfo] =
          bulletPath.collect { case pos if brickPositions.contains(pos) => brickWalls.get(pos) }.flatten.toList
            .map(wall => CollisionInfo(bullet, dist(bullet, wall.position), wall.position, wall))

        val bulletCollisions: List[CollisionInfo] =
          (for {
            (otherBullet, otherBulletPath) <- bulletsMovement
            if otherBullet.id != bullet.id
          } yield {
            bulletPath
              .zip(otherBulletPath)
              .collectFirst {
                case ((bulletX, bulletY), point @ (otherBulletX, otherBulletY))
                    if (bulletX - 2 to bulletX + 2).contains(otherBulletX) && (bulletY - 2 to bulletY + 2).contains(
                      otherBulletY) =>
                  CollisionInfo(bullet, dist(bullet, point), point, otherBullet)
              }
          }).flatten

        tankCollisions ++ wallCollisions ++ bulletCollisions
    }
  }

  private def findCollisions(collisions: List[CollisionInfo]): Map[Bullet, CollisionInfo] = {
    @tailrec
    def loop(colls: List[CollisionInfo], acc: Map[Bullet, CollisionInfo]): Map[Bullet, CollisionInfo] = {
      if (colls.isEmpty) acc
      else {
        val minimumCollisions: List[CollisionInfo] =
          colls.groupBy(_.obj).map { case (_, collisionInfos) => collisionInfos.minBy(_.distance) }.toList

        val minimumPerBullet: Map[Bullet, CollisionInfo] =
          minimumCollisions
            .groupBy(_.bullet)
            .mapValues(_.minBy(_.distance))

        val resolvedBullets = minimumPerBullet.keySet

        val collisionsLeft = collisions.filterNot {
          case CollisionInfo(bullet, _, _, _) =>
            resolvedBullets.contains(bullet)
        }

        loop(collisionsLeft, acc ++ minimumPerBullet)
      }
    }

    loop(collisions, Map.empty)
  }

  private def updateCollidedObjects(
    collisions: Map[Bullet, CollisionInfo]): (List[Tank], List[Bullet], List[BrickWall]) = {
    val (destroyedTanks, destroyedBullets1) = collisions.collect {
      case (bullet, CollisionInfo(_, _, point, tank: Tank)) =>
        val t = tank.copy(position = point)
        val b = bullet.copy(position = point)
        (t, b)
    }.toList.unzip

    val destroyedBullets2 = collisions.collect {
      case (bullet, CollisionInfo(_, _, point, otherBullet: Bullet)) =>
        val b1 = bullet.copy(position = point)
        val b2 = otherBullet.copy(position = point)

        List(b1, b2)
    }.flatten

    val (destroyedWalls, destroyedBullets3) = collisions.collect {
      case (bullet, CollisionInfo(_, _, point, wall: BrickWall)) =>
        val b = bullet.copy(position = point)
        val w = wall.copy(hitDirection = Some(b.direction), hp = wall.hp - 1)
        (w, b)
    }.toList.unzip

    (destroyedTanks, destroyedBullets1 ++ destroyedBullets2 ++ destroyedBullets3, destroyedWalls)
  }
}
