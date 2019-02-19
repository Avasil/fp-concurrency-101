package exercises
import cats.effect.concurrent.{Deferred, Semaphore}
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.exceptions.DummyException

import scala.concurrent.duration._

object Exercises extends App {
  /*
    1. Run `tasks` in parallel and cancel all of them in case of any error!
    All tasks should print "start" but no task should print "end".

    TIP: Don't overthink this, there is a function with thi behaviour by default
   */

  def foo(i: Int): Task[Unit] =
    for {
      _ <- Task(println(s"start $i"))
      _ <- if (i % 2 == 0) Task.raiseError(DummyException("error")) else Task.sleep(1.second)
      _ <- Task(println(s"end $i"))
    } yield ()

  val tasks: List[Task[Unit]] = List(foo(1), foo(2), foo(3), foo(4), foo(5))

  // 1. Solution:

//  Task.gather(tasks).attempt.runSyncUnsafe()

  /*
   2. Now try running `tasks` in parallel but don't cancel anything in case of errors!
   Then count all the successes.

   TIP: You can take a look at `Task#guaranteeCase` for a way to check if something got
        cancel signal or not.
   */

  // 2. Solution

  val count = Task.wander(tasks)(_.attempt).map(_.count(_.isRight))

//  println(count.runSyncUnsafe())

  /*
  3. Let's complicate it further: Now write a function that will run `tasks2` and `otherTask` in
    parallel but will cancel `otherTask` ONLY IF there were more than 3 successes in `tasks`.

    Expected result: (order might vary)

    start other
    start 1
    start 3
    start 5
    start 7
    start 2
    end 1
    end 3
    end 5
    end 7

    TIP: You'll probably have to use something for synchronization between two tasks
   */

  val tasksEx3 = List(foo(1), foo(3), foo(5), foo(7), foo(2))

  val otherTask =
    for {
      _ <- Task(println("start other"))
      _ <- Task.sleep(5.second)
      _ <- Task(println("end other"))
    } yield ()

  // 3. Solution
  val solution3 =
    for {
      deferred <- Deferred[Task, Unit]
      _ <- Task
        .wander(tasksEx3)(_.attempt)
        .flatMap(results => if (results.count(_.isRight) > 3) deferred.complete(()) else Task.unit)
        .start
      _ <- Task.race(otherTask, deferred.get)
    } yield ()

//  solution3.runSyncUnsafe()

  /*
   4. You're probably quite familiar with `parTraverse` / `wander` and similar functions by now.
      They try to execute all tasks in parallel but what if we want to limit the number of tasks
      in parallel?

      See if you can implement such a function!

      TIP: Take a look at `cats.effect.concurrent.Semaphore`
   */

  // 4. Solution
  def parTraverseBounded[A, B](n: Int, seq: List[A])(f: A => Task[B]): Task[List[B]] =
    for {
      s       <- Semaphore[Task](n)
      results <- Task.wander(seq)(a => s.withPermit(f(a)))
    } yield results

  /*
   5. There are operations like `race`, `parTraverse` and so on that will
      cancel other tasks in case of error.

      It could be cool to see if we can reverse this semantics and use existing
      combinators to write a function that will work until the first success, ignoring
      any errors.
   */

  val timeout = 30.second
  val task1   = Task.eval(10).delayExecution(3.second)
  val task2   = Task.raiseError[Int](DummyException("error")).delayExecution(2.second)
  val task3   = Task.raiseError[Int](DummyException("error")).delayExecution(1.second)

  val tasksEx5: List[Task[Int]] = List(task1, task2, task3)
  val result5: Task[Int]        = Task.raceMany(tasksEx5.map(_.onErrorHandleWith(_ => Task.never))).timeout(timeout)

  println(result5.runSyncUnsafe()) // should print 10 after 3 seconds

  // 5.1 Make sure it works on the following input:
  val task4 = Task.raiseError[Int](DummyException("error")).delayExecution(3.second)
  val task5 = Task.raiseError[Int](DummyException("error")).delayExecution(2.second)
  val task6 = Task.raiseError[Int](DummyException("error")).delayExecution(1.second)

  val tasksEx52: List[Task[Int]] = List(task4, task5, task6)

  // 5.2 Make sure it works on inputs like the above but only takes up to n seconds
  //     where n is the duration of the longest task

  val semaphore = Semaphore[Task](0)

  val result52: Task[Either[Unit, Int]] = semaphore.flatMap { sem =>
    Task.race(
      sem.acquireN(tasksEx52.length),
      Task.raceMany(tasksEx52.map(_.onErrorHandleWith(_ => sem.release >> Task.never)))
    )
  }

  println(result52.runSyncUnsafe()) // should finish and print after 3 seconds

  // ======================= Time for playing with Tanks ! =======================

  /*
   6.   Had fun solving these puzzles, finished the rest of the workshop and looking for
        bigger challenge? Check out Scala Exercises by Oleg Pyzhcov:
        https://olegpy.com/cats-effect-exercises/
 */
}
