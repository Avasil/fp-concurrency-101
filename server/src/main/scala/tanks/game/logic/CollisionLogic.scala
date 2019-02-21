package tanks.game.logic
import shared.models.GameObject._
import shared.models.{Direction, EnvObject, GameObject, GameState}
import tanks.game.logic.CollisionInfo.dist

import scala.annotation.tailrec

trait CollisionLogic {

  protected def resolveCollisions(gameState: GameState): (Destroyed, GameState) = {
    val GameState(tanks, _, environment) = gameState
    val bullets                          = moveBullets(gameState.bullets)

    val collisionInfo: List[CollisionInfo] = getAllPossibleCollisions(tanks, bullets, environment)

    val collisionsByBullet: Map[Bullet, CollisionInfo] = resolveConflictingCollisions(collisionInfo)

    val (updatedTanks, updatedBullets, updatedEnv) = updateCollidedObjects(collisionsByBullet)

    val newTanks       = tanks ++ updatedTanks.map(tank => tank.id         -> tank).toMap
    val newBullets     = bullets ++ updatedBullets.map(bullet => bullet.id -> bullet).toMap
    val newEnvironment = updatedEnv.map(env => env.destination             -> env).toMap

    val destroyed =
      Destroyed(updatedTanks.map(_.id), updatedBullets.map(_.id), updatedEnv.filter(_.hp <= 0).map(_.destination))

    destroyed -> GameState(newTanks, newBullets, newEnvironment)
  }

  private def moveBullets(bullets: Map[Int, Bullet]): Map[Int, Bullet] = {

    val updatedBullets = bullets.mapValues {
      case b @ Bullet(_, _, (x, y), _, direction) =>
        val (destX, destY) = direction match {
          case Direction.UP    => (x, y - 32)
          case Direction.DOWN  => (x, y + 32)
          case Direction.LEFT  => (x - 32, y)
          case Direction.RIGHT => (x + 32, y)
        }

        b.copy(destination = (destX, destY), prevPosition = (x, y))
    }

    updatedBullets
  }

  private def getAllPossibleCollisions(
    tanks: Map[Int, Tank],
    bullets: Map[Int, Bullet],
    environment: Map[(Int, Int), EnvObject]
  ): List[CollisionInfo] = {
    val tanksMovement: List[(Tank, Stream[(Int, Int)])] = tanks.mapValues {
      case t @ Tank(_, _, dest, prev, _) if dest == prev =>
        t -> Stream.continually(dest)
      case t @ Tank(_, _, (posX, posY), (prevX, prevY), _) =>
        t -> GameObject.movementCoords(prevX, prevY, posX, posY, 16).toStream
    }.values.toList

    val walls: Map[(Int, Int), EnvObject] = environment.collect {
      case (pos, b: BrickWall) => (pos, b)
      case (pos, s: SteelWall) => (pos, s)
    }
    val wallsPositions: Set[(Int, Int)] = walls.keySet

    val bulletsMovement: List[(Bullet, Seq[(Int, Int)])] = bullets.mapValues {
      case b @ Bullet(_, _, (posX, posY), (prevX, prevY), _) =>
        b -> GameObject.movementCoords(prevX, prevY, posX, posY, 16)
    }.values.toList

    bulletsMovement.flatMap {
      case (bullet, bulletPath) =>
        val tankCollisions: List[CollisionInfo] =
          (for { (tank, tankPath) <- tanksMovement } yield {
            tankPath
              .zip(bulletPath)
              .collectFirst {
                case (tankCoords, bulletCoords) if tankCoords == bulletCoords =>
                  CollisionInfo(bullet, dist(bullet, tankCoords), tankCoords, tank)
              }
          }).flatten

        val wallCollisions: List[CollisionInfo] =
          bulletPath.collect { case pos if wallsPositions.contains(pos) => walls.get(pos) }.flatten.toList
            .map(wall => CollisionInfo(bullet, dist(bullet, wall.destination), wall.destination, wall))

        val bulletCollisions: List[CollisionInfo] =
          (for {
            (otherBullet, otherBulletPath) <- bulletsMovement
            if otherBullet.id != bullet.id
          } yield {
            bulletPath
              .zip(otherBulletPath)
              .collectFirst {
                case (bulletCoords, otherBulletCoords) if otherBulletCoords == bulletCoords =>
                  CollisionInfo(bullet, dist(bullet, otherBulletCoords), otherBulletCoords, otherBullet)
              }
          }).flatten

        tankCollisions ++ wallCollisions ++ bulletCollisions
    }
  }

  private def resolveConflictingCollisions(collisions: List[CollisionInfo]): Map[Bullet, CollisionInfo] = {
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

        val collisionsLeft = colls.filterNot {
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
      case (bullet, CollisionInfo(_, _, point, tank: Tank))
          // to prevent stopping at the point of firing
          if bullet.team == tank.team && bullet.prevPosition != point =>
        val b = bullet.copy(destination = point)
        (None, b)
      case (bullet, CollisionInfo(_, _, point, tank: Tank)) if bullet.team != tank.team =>
        val t = tank.copy(destination = point)
        val b = bullet.copy(destination = point)
        (Some(t), b)
    }.toList.unzip

    val destroyedBullets2 = collisions.collect {
      case (bullet, CollisionInfo(_, _, point, otherBullet: Bullet)) =>
        val b1 = bullet.copy(destination = point)
        val b2 = otherBullet.copy(destination = point)

        List(b1, b2)
    }.flatten

    val (destroyedWalls, destroyedBullets3) = collisions.collect {
      case (bullet, CollisionInfo(_, _, point, wall: BrickWall)) =>
        val b = bullet.copy(destination = point)
        val w = wall.copy(hitDirection = Some(b.direction), hp = wall.hp - 1)
        (w, b)
    }.toList.unzip

    val destroyedBullets4 = collisions.collect {
      case (bullet, CollisionInfo(_, _, point, _: SteelWall)) =>
        bullet.copy(destination = point)
    }.toList

    (destroyedTanks.flatten,
     destroyedBullets1 ++ destroyedBullets2 ++ destroyedBullets3 ++ destroyedBullets4,
     destroyedWalls)
  }
}
