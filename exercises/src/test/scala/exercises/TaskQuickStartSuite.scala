package exercises

import cats.effect.ExitCase
import exercises.TaskQuickStart.{
  Cancellation,
  Composing,
  Concurrency,
  ConcurrencyPrimitives,
  CreatingAndRunning,
  ErrorHandling,
  Parallelism
}
import monix.execution.exceptions.DummyException
import exercises.TaskQuickStart.CreatingAndRunning.Sum
import monix.eval.Task
import monix.execution.atomic.AtomicInt
import monix.execution.schedulers.TestScheduler
import monix.execution.{ExecutionModel, Scheduler}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object TaskQuickStartSuite extends BaseTestSuite {
  test("CreatingAndRunning.ex2 (success)") { implicit s =>
    val sum = new Sum
    val f   = CreatingAndRunning.ex2(sum).runToFuture

    sum.onNext(1)
    sum.onNext(2)
    assertEquals(f.value, None)

    sum.onComplete()
    assertEquals(f.value, Some(Success(3)))
  }

  test("CreatingAndRunning.ex2 (failure)") { implicit s =>
    val sum   = new Sum
    val f     = CreatingAndRunning.ex2(sum).runToFuture
    val dummy = DummyException("boom!")

    assertEquals(f.value, None)

    sum.onError(dummy)
    assertEquals(f.value, Some(Failure(dummy)))
  }

  test("Composing.ex1") { implicit s =>
    val a = Task(10)
    val b = Task(20)

    val f = Composing.ex1(a, b).runToFuture

    assertEquals(f.value, Some(Success(30)))
  }

  test("Composing.ex2") { implicit s =>
    def compute: Int => Task[Int] = i => Task.now(i * 2)
    val numbers                   = List(1, 2, 3, 4, 5, 6, 7)

    val f = Composing.ex2(numbers, compute).runToFuture

    assertEquals(f.value, Some(Success(numbers.sum * 2)))
  }

  test("ErrorHandling.ex1 (success)") { implicit s =>
    val task = Task("success")

    val f = ErrorHandling.ex1(task).runToFuture

    assertEquals(f.value, Some(Success(Right("success"))))
  }

  test("ErrorHandling.ex1 (failure)") { implicit s =>
    val dummy = DummyException("dummy")
    val task  = Task.raiseError(dummy)

    val f = ErrorHandling.ex1(task).runToFuture

    assertEquals(f.value, Some(Success(Left(dummy))))
  }

  test("ErrorHandling.ex2 (success)") { implicit s =>
    val dummy = DummyException("dummy")
    var tries = 0
    val task = Task.suspend {
      tries += 1
      if (tries == 3) Task.now(10)
      else Task.raiseError(dummy)
    }

    val f = ErrorHandling.ex2(task, 3, 100.millis).runToFuture

    assertEquals(f.value, None)
    assertEquals(tries, 1)

    s.tick(100.millis)
    assertEquals(f.value, None)
    assertEquals(tries, 2)

    s.tick(200.millis)
    assertEquals(f.value, Some(Success(10)))
    assertEquals(tries, 3)
  }

  test("ErrorHandling.ex2 (failure)") { implicit s =>
    val dummy = DummyException("dummy")
    var tries = 0
    val task = Task.suspend {
      tries += 1
      Task.raiseError(dummy)
    }

    val f = ErrorHandling.ex2(task, 3, 100.millis).runToFuture

    assertEquals(f.value, None)
    assertEquals(tries, 1)

    s.tick(100.millis)
    assertEquals(f.value, None)
    assertEquals(tries, 2)

    s.tick(200.millis)
    assertEquals(f.value, None)
    assertEquals(tries, 3)

    s.tick(400.millis)
    assertEquals(f.value, Some(Failure(dummy)))
    assertEquals(tries, 4)
  }

  test("Concurrency.ex1") { _ =>
    implicit val s: Scheduler = Scheduler
      .singleThread("test")
      .withExecutionModel(ExecutionModel.SynchronousExecution)

    val dummy = DummyException("dummy")
    val task  = Task.raiseError(dummy).executeAsync

    val f = Concurrency.ex1(task).attempt.runToFuture

    assertEquals(Await.result(f, 2.second), Left(dummy))
  }

  test("Concurrency.ex2") { implicit s =>
    val s2 = TestScheduler()

    var effect         = 0
    var blockingEffect = 0

    val normalTask   = Task { effect += 1 }
    val blockingTask = Task { blockingEffect += 1 }

    val f = Concurrency.ex2(normalTask, blockingTask, s2).runToFuture

    // first normalTask will run synchronously
    assertEquals(f.value, None)
    assertEquals(effect, 1)
    assertEquals(blockingEffect, 0)

    // execute blockingTask
    s2.tick()
    assertEquals(f.value, None)
    assertEquals(effect, 1)
    assertEquals(blockingEffect, 1)

    // execute second normalTask
    s.tick()
    assertEquals(f.value, Some(Success(())))
    assertEquals(effect, 2)
    assertEquals(blockingEffect, 1)
  }

  test("Parallelism.ex1") { implicit s =>
    def foo(i: Int): Task[Unit] =
      if (i % 2 == 0) Task.raiseError(DummyException("error"))
      else Task.sleep(1.second)

    val tasks: List[Task[Unit]] = List(foo(1), foo(2), foo(3), foo(4), foo(5))

    val f = Parallelism.ex1(tasks).map(_ => ()).runToFuture

    s.tick(1.second)
    assertEquals(f.value, Some(Success(())))
    assertEquals(s.state.lastReportedError, null)

  }

  test("Parallelism.ex2") { implicit s =>
    def foo(i: Int): Task[Unit] =
      if (i % 2 == 0) Task.raiseError(DummyException("error"))
      else Task.sleep(1.second)

    val tasks: List[Task[Unit]] = List(foo(1), foo(2), foo(3), foo(4), foo(5))

    val f = Parallelism.ex2(tasks).runToFuture

    s.tick(1.second)
    assertEquals(f.value, Some(Success((3, 2))))
    assertEquals(s.state.lastReportedError, null)
  }

  test("Cancellation.ex1 (success)") { implicit s =>
    var cancelledA = false
    var completedA = false

    var cancelledB = false
    var completedB = false

    val ta = Task.sleep(2.second).guaranteeCase {
      case ExitCase.Completed => Task { completedA = true }
      case ExitCase.Error(_)  => Task.unit
      case ExitCase.Canceled  => Task { cancelledA = true }
    }

    val tb = Task.sleep(3.second).guaranteeCase {
      case ExitCase.Completed => Task { completedB = true }
      case ExitCase.Error(_)  => Task.unit
      case ExitCase.Canceled  => Task { cancelledB = true }
    }

    Cancellation.ex1(ta, tb).runAsyncAndForget

    s.tick(3.second)

    assertEquals(completedA, true)
    assertEquals(cancelledA, false)

    assertEquals(completedB, false)
    assertEquals(cancelledB, true)
  }

  test("Cancellation.ex1 (failure)") { implicit s =>
    val dummy = DummyException("boom")

    var cancelledA = false
    var completedA = false
    var erroredA   = false

    var cancelledB = false
    var completedB = false
    var erroredB   = false

    val ta = Task.sleep(3.second).guaranteeCase {
      case ExitCase.Completed => Task { completedA = true }
      case ExitCase.Error(_)  => Task { erroredA = true }
      case ExitCase.Canceled  => Task { cancelledA = true }
    }

    val tb = Task.raiseError(dummy).delayExecution(2.second).guaranteeCase {
      case ExitCase.Completed => Task { completedB = true }
      case ExitCase.Error(_)  => Task { erroredB = true }
      case ExitCase.Canceled  => Task { cancelledB = true }
    }

    Cancellation.ex1(ta, tb).runAsyncAndForget

    s.tick(3.second)

    assertEquals(completedA, true)
    assertEquals(cancelledA, false)
    assertEquals(erroredA, false)

    assertEquals(completedB, false)
    assertEquals(cancelledB, false)
    assertEquals(erroredB, true)
  }

  test("Cancellation.ex2 (success)") { implicit s =>
    var cancelledA = false
    var completedA = false

    var cancelledB = false
    var completedB = false

    val ta = Task.sleep(2.second).guaranteeCase {
      case ExitCase.Completed => Task { completedA = true }
      case ExitCase.Error(_)  => Task.unit
      case ExitCase.Canceled  => Task { cancelledA = true }
    }

    val tb = Task.sleep(3.second).guaranteeCase {
      case ExitCase.Completed => Task { completedB = true }
      case ExitCase.Error(_)  => Task.unit
      case ExitCase.Canceled  => Task { cancelledB = true }
    }

    Cancellation.ex2(ta, tb)

    s.tick(3.second)

    assertEquals(completedA, true)
    assertEquals(cancelledA, false)

    assertEquals(completedB, false)
    assertEquals(cancelledB, true)
  }

  test("Cancellation.ex2 (failure)") { implicit s =>
    val dummy = DummyException("boom")

    var cancelledA = false
    var completedA = false
    var erroredA   = false

    var cancelledB = false
    var completedB = false
    var erroredB   = false

    val ta = Task.sleep(3.second).guaranteeCase {
      case ExitCase.Completed => Task { completedA = true }
      case ExitCase.Error(_)  => Task { erroredA = true }
      case ExitCase.Canceled  => Task { cancelledA = true }
    }

    val tb = Task.raiseError(dummy).delayExecution(2.second).guaranteeCase {
      case ExitCase.Completed => Task { completedB = true }
      case ExitCase.Error(_)  => Task { erroredB = true }
      case ExitCase.Canceled  => Task { cancelledB = true }
    }

    Cancellation.ex2(ta, tb)

    s.tick(3.second)

    assertEquals(completedA, true)
    assertEquals(cancelledA, false)
    assertEquals(erroredA, false)

    assertEquals(completedB, false)
    assertEquals(cancelledB, false)
    assertEquals(erroredB, true)
  }

  test("Cancellation.ex3 (success)") { implicit s =>
    var cancelledA = false
    var completedA = false

    var cancelledB = false
    var completedB = false

    val ta = Task.sleep(2.second).guaranteeCase {
      case ExitCase.Completed => Task { completedA = true }
      case ExitCase.Error(_)  => Task.unit
      case ExitCase.Canceled  => Task { cancelledA = true }
    }

    val tb = Task.sleep(3.second).guaranteeCase {
      case ExitCase.Completed => Task { completedB = true }
      case ExitCase.Error(_)  => Task.unit
      case ExitCase.Canceled  => Task { cancelledB = true }
    }

    Cancellation.ex3(ta, tb).runAsyncAndForget

    s.tick(3.second)

    assertEquals(completedA, true)
    assertEquals(cancelledA, false)

    assertEquals(completedB, false)
    assertEquals(cancelledB, true)
  }

  test("Cancellation.ex3 (failure)") { implicit s =>
    val dummy = DummyException("boom")

    var cancelledA = false
    var completedA = false
    var erroredA   = false

    var cancelledB = false
    var completedB = false
    var erroredB   = false

    val ta = Task.sleep(3.second).guaranteeCase {
      case ExitCase.Completed => Task { completedA = true }
      case ExitCase.Error(_)  => Task { erroredA = true }
      case ExitCase.Canceled  => Task { cancelledA = true }
    }

    val tb = Task.raiseError(dummy).delayExecution(2.second).guaranteeCase {
      case ExitCase.Completed => Task { completedB = true }
      case ExitCase.Error(_)  => Task { erroredB = true }
      case ExitCase.Canceled  => Task { cancelledB = true }
    }

    Cancellation.ex3(ta, tb).runAsyncAndForget

    s.tick(3.second)

    assertEquals(completedA, true)
    assertEquals(cancelledA, false)
    assertEquals(erroredA, false)

    assertEquals(completedB, false)
    assertEquals(cancelledB, false)
    assertEquals(erroredB, true)
  }

  test("ConcurrencyPrimitives.ex1") { implicit s =>
    val f =
      for {
        (ref, task) <- ConcurrencyPrimitives.ex1
        fiber       <- task.start
        v1          <- ref.get
        _           <- Task(assertEquals(v1, 0))
        _ = s.tick(2.second)
        v2 <- ref.get
        _  <- Task(assertEquals(v2, 1))
        _ = s.tick(1.second)
        v3 <- ref.get
        _  <- Task(assertEquals(v3, 2))
        _ = s.tick(1.second)
        v4 <- ref.get
        _  <- Task(assertEquals(v4, 3))
        _ = s.tick(2.second)
        v5 <- ref.get
        _  <- Task(assertEquals(v5, 8)).onErrorHandle(_ => assertEquals(v5, 7))
        _  <- fiber.cancel
      } yield ()

    f.runSyncUnsafe()
  }

  test("ConcurrencyPrimitives.ex2") { implicit s =>
    val num            = AtomicInt(0)
    val seq            = List.fill(100)(())
    val fn: Task[Unit] = Task.evalAsync(num.increment()) >> Task.sleep(2.seconds)

    ConcurrencyPrimitives.ex2(5, seq)(_ => fn).runAsyncAndForget

    s.tick()
    assertEquals(num.get(), 5)
    s.tick(2.seconds)
    assertEquals(num.get(), 10)
    s.tick(4.seconds)
    assertEquals(num.get(), 20)
    s.tick(34.seconds)
    assertEquals(num.get(), 100)
  }

  test("ConcurrencyPrimitives.ex3 cancels for 3 errors") { implicit s =>
    def foo(i: Int): Task[Unit] =
      if (i % 2 == 0) Task.raiseError(DummyException("error")).delayExecution(i.second)
      else Task.sleep(1.second)

    val tasks: List[Task[Unit]] = List(foo(1), foo(2), foo(3), foo(4), foo(5))

    var canceled              = false
    val otherTask: Task[Unit] = Task.never.doOnCancel(Task { canceled = true })

    ConcurrencyPrimitives.ex3(tasks, otherTask).runAsyncAndForget

    s.tick(1.second)
    assertEquals(canceled, false)

    s.tick(2.second)
    assertEquals(canceled, false)

    s.tick(2.second)
    assertEquals(canceled, true)
  }

  test("ConcurrencyPrimitives.ex3 does not cancel for less than 3 errors") { implicit s =>
    def foo(i: Int): Task[Unit] =
      if (i % 2 == 0) Task.raiseError(DummyException("error")).delayExecution(i.second)
      else Task.sleep(1.second)

    val tasks: List[Task[Unit]] = List(foo(1), foo(2), foo(3), foo(4))

    var canceled              = false
    val otherTask: Task[Unit] = Task.never.doOnCancel(Task { canceled = true })

    ConcurrencyPrimitives.ex3(tasks, otherTask).runAsyncAndForget

    s.tick(1.day)
    assertEquals(canceled, false)
  }

}
