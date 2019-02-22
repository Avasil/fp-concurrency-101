package exercises
import cats.effect.concurrent.{Deferred, Semaphore}
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.exceptions.DummyException

import scala.concurrent.duration._

/*
 This section contains several concurrency "puzzles" that should get you familiar with
 combining different functions and share state through concurrent data structures.

 Hopefully you can notice how easy it is to customize your `Task`, reusing old pieces and adding
 more and more complex behavior.
 */

object Exercises extends App {
  /*
    1. Run `tasks` in parallel and cancel all of them in case of any error!
    All tasks should print "start" but no task should print "end".

    TIP: Don't overthink this, there is a function with this behaviour by default.
         I want to encourage you to practice navigating in the API so take a look:
         - https://monix.io/api/3.0/monix/eval/Task.html
         - https://github.com/monix/monix/blob/master/monix-eval/shared/src/main/scala/monix/eval/Task.scala
         - ..or the code in your IDE. :)
   */

  def foo(i: Int): Task[Unit] =
    for {
      _ <- Task(println(s"start $i"))
      _ <- if (i % 2 == 0) Task.raiseError(DummyException("error")) else Task.sleep(1.second)
      _ <- Task(println(s"end $i"))
    } yield ()

  val tasks: List[Task[Unit]] = List(foo(1), foo(2), foo(3), foo(4), foo(5))

  /*
   2. Now try running `tasks` in parallel but don't cancel anything in case of errors!
   Finally, count all the successes.
   */

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

    TIP: You can take a look at `Task#guaranteeCase` for a way to check if something got
        cancel signal or not.
    TIP: You'll probably have to use something for synchronization between two tasks. Usually it means either
         `Deferred` or `MVar`.
   */

  val tasksEx3 = List(foo(1), foo(3), foo(5), foo(7), foo(2))

  val otherTask =
    for {
      _ <- Task(println("start other"))
      _ <- Task.sleep(5.second)
      _ <- Task(println("end other"))
    } yield ()

  /*
   4. You're probably quite familiar with `parTraverse` / `wander` and similar functions by now.
      They try to execute all tasks in parallel but what if we want to limit the number of tasks
      running in parallel (go through the list in batches)?

      See if you can implement such a function!

      TIP: Take a look at `cats.effect.concurrent.Semaphore`
   */

  def parTraverseBounded[A, B](n: Int, seq: List[A])(f: A => Task[B]): Task[List[B]] = ???

  /*
   5. There are operations like `race`, `parTraverse` and so on that will
      cancel other tasks in case of error.

      It would be cool to see if we can reverse the semantics and use existing
      methods to write a function that will work until the first success, ignoring
      any errors.
   */

  val task1 = Task.eval(10).delayExecution(3.second)
  val task2 = Task.raiseError[Int](DummyException("error")).delayExecution(2.second)
  val task3 = Task.raiseError[Int](DummyException("error")).delayExecution(1.second)

  val tasksEx5: List[Task[Int]] = List(task1, task2, task3)
  val result5: Task[Int]        = ???

  println(result5.runSyncUnsafe()) // should print 10 after 3 seconds

  // 5.1 Make sure it works on the following input:
  val task4 = Task.raiseError[Int](DummyException("error")).delayExecution(3.second)
  val task5 = Task.raiseError[Int](DummyException("error")).delayExecution(2.second)
  val task6 = Task.raiseError[Int](DummyException("error")).delayExecution(1.second)

  val tasksEx52: List[Task[Int]] = List(task4, task5, task6)

  // 5.2 Make sure it works on inputs like the above but only takes up to n seconds
  //     where n is the duration of the longest task

  val result52: Task[Either[Unit, Int]] = ???

  println(result52.runSyncUnsafe()) // should finish and print after 3 seconds

  // ======================= Time for playing with Tanks ! =======================

  /*
   6.   Had fun solving these puzzles, finished the rest of the workshop and looking for
        bigger challenge? Check out Scala Exercises by Oleg Pyzhcov:
        https://olegpy.com/cats-effect-exercises/
 */
}
