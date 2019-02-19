package tanks.game.logic
import monix.execution.atomic.AtomicInt
import shared.models.GameObject.{Bullet, Tank}
import shared.models.{Direction, Movement, MovementCommand}

trait TankMovement {
  protected def resolveTankMovement(players: Map[Int, Tank],
                                    bullets: Map[Int, Bullet],
                                    commands: Seq[MovementCommand]): (Map[Int, Tank], Map[Int, Bullet]) = {

    val (updatedPlayers, updatedBullets) = commands.foldLeft((players, bullets)) {
      case ((playersAcc, bulletsAcc), MovementCommand(id, movement)) =>
        playersAcc.get(id).fold((playersAcc, bulletsAcc)) { tank =>
          val (x, y) = tank.position

          val updatedTank = movement match {
            case Movement.Fire =>
              tank.copy(prevPosition = tank.position)
            case Movement.Up =>
              tank.copy(direction = Direction.UP, prevPosition = tank.position, position = (x, y - 16))
            case Movement.Down =>
              tank.copy(direction = Direction.DOWN, prevPosition = tank.position, position = (x, y + 16))
            case Movement.Right =>
              tank.copy(direction = Direction.RIGHT, prevPosition = tank.position, position = (x + 16, y))
            case Movement.Left =>
              tank.copy(direction = Direction.LEFT, prevPosition = tank.position, position = (x - 16, y))
          }

          val newBullets: Map[Int, Bullet] =
            if (movement.isInstanceOf[Movement.Fire.type]) {
              val bullet = createBullet(tank)
              Map(bullet.id -> bullet)
            } else {
              Map.empty
            }
          (playersAcc ++ Map(tank.id -> updatedTank), bulletsAcc ++ newBullets)
        }
    }
    (updatedPlayers, updatedBullets)
  }

  private def createBullet(shooter: Tank): Bullet = {
    val (x, y) = shooter.position
    val position = shooter.direction match {
      case Direction.UP    => (x, y - 16)
      case Direction.DOWN  => (x, y + 16)
      case Direction.LEFT  => (x - 16, y)
      case Direction.RIGHT => (x + 16, y)
    }

    Bullet(newId(), position, position, shooter.direction)
  }

  private val lastBulletId = AtomicInt(0)

  private[this] def newId(): Int = {
    lastBulletId.getAndIncrement()
  }

}
