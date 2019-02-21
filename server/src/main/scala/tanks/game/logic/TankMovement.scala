package tanks.game.logic

import monix.execution.atomic.AtomicInt
import shared.models.GameObject.{Bullet, Tank}
import shared.models.{Direction, EnvObject, Movement, MovementCommand}

trait TankMovement {
  protected def moveTank(
    players: Map[Int, Tank],
    bullets: Map[Int, Bullet],
    environment: Map[(Int, Int), EnvObject],
    commands: Seq[MovementCommand]
  ): (Map[Int, Tank], Map[Int, Bullet]) = {

    val (updatedPlayers, updatedBullets) =
      commands.foldLeft((players.mapValues(t => t.copy(prevPosition = t.destination)), bullets)) {
        case ((playersAcc, bulletsAcc), MovementCommand(id, movement)) =>
          playersAcc.get(id).fold((playersAcc, bulletsAcc)) { tank =>
            val (x, y) = tank.destination

            val updatedTank = movement match {
              case Movement.Fire =>
                tank.copy(prevPosition = tank.destination)
              case Movement.Up =>
                tank.copy(direction = Direction.UP, destination = (x, y - 16))
              case Movement.Down =>
                tank.copy(direction = Direction.DOWN, destination = (x, y + 16))
              case Movement.Right =>
                tank.copy(direction = Direction.RIGHT, destination = (x + 16, y))
              case Movement.Left =>
                tank.copy(direction = Direction.LEFT, destination = (x - 16, y))
            }

            val otherPlayersDestination = (playersAcc - id).values.map(_.destination).toSet

            val resultTank: Tank =
              if (otherPlayersDestination.contains(updatedTank.destination)) tank
              else updatedTank

            val newBullets: Map[Int, Bullet] =
              if (movement.isInstanceOf[Movement.Fire.type]) {
                val bullet = createBullet(tank)
                Map(bullet.id -> bullet)
              } else {
                Map.empty
              }
            (playersAcc ++ Map(tank.id -> resultTank), bulletsAcc ++ newBullets)
          }
      }

    val finalPlayers: Map[Int, Tank] = updatedPlayers.mapValues { t: Tank =>
      if (environment.contains(t.destination)) t.copy(destination = t.prevPosition)
      else t
    }
    (finalPlayers, updatedBullets)
  }

  private def createBullet(shooter: Tank): Bullet = {
    val (x, y) = shooter.destination
    val position = shooter.direction match {
      case Direction.UP    => (x, y - 16)
      case Direction.DOWN  => (x, y + 16)
      case Direction.LEFT  => (x - 16, y)
      case Direction.RIGHT => (x + 16, y)
    }

    Bullet(newId(), shooter.team, position, shooter.destination, shooter.direction)
  }

  private val lastBulletId = AtomicInt(0)

  private[this] def newId(): Int = {
    lastBulletId.getAndIncrement()
  }

}
