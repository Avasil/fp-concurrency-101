package exercises

import exercises.ObservableQuickStart.BufferingAndThrottling.Input
import exercises.ObservableQuickStart.{BufferingAndThrottling, CreatingAndRunning, Transforming}
import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.execution.Ack.Continue
import monix.execution.BufferCapacity.Unbounded
import monix.execution.ChannelType.MPMC
import monix.execution.exceptions.DummyException
import monix.reactive.Observable

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object ObservableQuickStartSuite extends BaseTestSuite {
  test("CreatingAndRunning.ex1") { implicit s =>
    var lastLong = -1L

    val cancelable = CreatingAndRunning.ex1.subscribe { next =>
      lastLong = next; Continue
    }

    s.tick(5.second)
    assertEquals(lastLong, 1L)

    s.tick(5.second)
    assertEquals(lastLong, 2L)

    s.tick(5.second)
    assertEquals(lastLong, 3L)

    s.tick(5.second)
    assertEquals(lastLong, 4L)

    cancelable.cancel()
  }

  test("CreatingAndRunning.ex2") { implicit s =>
    val list = List(1L, 2L, 3L, 4L)

    val f = CreatingAndRunning.ex2(Observable.fromIterable(list)).runToFuture

    assertEquals(f.value, Some(Success(list.product)))
  }

  test("CreatingAndRunning.ex3") { implicit s =>
    val queue = ConcurrentQueue.unsafe[Task, Long](Unbounded(None), MPMC)
    val f     = CreatingAndRunning.ex3(queue).runToFuture

    queue.offer(1L).runAsyncAndForget
    queue.offer(1L).runAsyncAndForget
    queue.offer(1L).runAsyncAndForget
    queue.offer(1L).runAsyncAndForget
    s.tick()
    assertEquals(f.value, None)

    queue.offer(0L).runAsyncAndForget
    s.tick()
    assertEquals(f.value, Some(Success(4)))
  }

  test("Transforming.ex1") { implicit s =>
    val source = Observable.range(1, 100)
    val f      = Transforming.ex1(source, _ % 2 == 0).toListL.runToFuture
    val f2     = source.filter(_ % 2 == 0).toListL.runToFuture

    assertEquals(f.value, f2.value)
  }

  test("Transforming.ex2") { implicit s =>
    var emitted = List.empty[Long]

    val f = Transforming.ex2.take(20).doOnNext(l => Task { emitted ::= l }).toListL.runToFuture

    s.tick()
    assertEquals(emitted.groupBy(identity).values.toList, List(List(0)))
    assertEquals(f.value, None)

    s.tick(1.second)
    assertEquals(emitted.groupBy(identity).values.toList, List(List(0, 0)))

    s.tick(1.second)
    assertEquals(emitted.groupBy(identity).values.toList, List(List(1), List(0, 0, 0)))

    s.tick(2.second)
    assertEquals(emitted.groupBy(identity).values.toList, List(List(2), List(1, 1, 1), List(0, 0, 0, 0)))

    s.tick(2.second)
    assertEquals(emitted.groupBy(identity).values.toList,
                 List(List(2, 2, 2), List(1, 1, 1, 1), List(3), List(0, 0, 0, 0)))

    s.tick(20.second)
    assertEquals(emitted.groupBy(identity).values.toList,
                 List(List(0, 0, 0, 0), List(5), List(1, 1, 1, 1), List(2, 2, 2, 2), List(3, 3, 3, 3), List(4, 4, 4)))
  }

  test("Transforming.ex3") { implicit s =>
    var emitted = List.empty[Long]
    val source  = Observable(1, 2, 3, -1, 1, 1)
    val dummy   = DummyException("boom")
    val f       = Transforming.ex3(source, dummy).foreachL(i => emitted ::= i).runToFuture

    s.tick()
    assertEquals(f.value, Some(Failure(dummy)))
    assertEquals(emitted, List(3, 2, 1))
  }

  test("Transforming.ex4") { implicit s =>
    var inputs1 = 0
    var inputs2 = 0

    val i1     = Input(1, "")
    val i2     = Input(2, "")
    val source = Observable(i1, i1, i1, i2, i2, i2, i1, i2, i1, i2)
    val process: Input => Task[Unit] =
      in => Task(if (in.playerId == 1) inputs1 += 1 else inputs2 += 1).delayExecution(1.second)

    Transforming.ex4(source, process).subscribe()

    s.tick(1.second)
    assertEquals(inputs1, 1)
    assertEquals(inputs2, 1)

    s.tick(1.second)
    assertEquals(inputs1, 2)
    assertEquals(inputs2, 2)

    s.tick(1.second)
    assertEquals(inputs1, 3)
    assertEquals(inputs2, 3)

    s.tick(1.second)
    assertEquals(inputs1, 4)
    assertEquals(inputs2, 4)

    s.tick(1.second)
    assertEquals(inputs1, 5)
    assertEquals(inputs2, 5)
  }

  test("BufferingAndThrottling.ex1") { implicit s =>
    var inputs1 = 0
    var inputs2 = 0

    val i1 = Input(1, "")
    val i2 = Input(2, "")

    val source = Observable(i1, i1, i1, i2) ++
      Observable(i2, i2, i2, i1).delayExecution(1500.milli) ++
      Observable(i2, i1, i2).delayExecution(1.second)

    val process: Seq[Input] => Task[Unit] =
      inputs =>
        Task
          .wander(inputs) { in =>
            Task(if (in.playerId == 1) inputs1 += 1 else inputs2 += 1)
          }
          .map(_ => ())

    BufferingAndThrottling.ex1(source, process).subscribe()

    s.tick(1.second)
    assertEquals(inputs1, 3)
    assertEquals(inputs2, 1)

    s.tick(1.second)
    assertEquals(inputs1, 4)
    assertEquals(inputs2, 4)

    s.tick(1.second)
    assertEquals(inputs1, 5)
    assertEquals(inputs2, 6)
  }

  test("BufferingAndThrottling.ex2") { implicit s =>
    var latestInput: Input = Input(999, "X")

    val source = Observable(Input(1, "A"), Input(1, "B"), Input(1, "B"), Input(1, "B")) ++
      Observable(Input(1, "C"), Input(1, "D"), Input(1, "E")).delayExecution(2.second) ++
      Observable(Input(1, "F"), Input(1, "G")).delayExecution(1.second)

    val process: Input => Task[Unit] =
      in => Task { latestInput = in }

    val f = BufferingAndThrottling.ex2(source, process).subscribe()

    s.tick()
    assertEquals(latestInput, Input(1, "A"))

    s.tick(1.second)
    assertEquals(latestInput, Input(1, "A"))

    s.tick(1.second)
    assertEquals(latestInput, Input(1, "C"))

    s.tick(1.second)
    assertEquals(latestInput, Input(1, "F"))
  }

  test("BufferingAndThrottling.ex3") { implicit s =>
    val sourceA =
      Observable.evalDelayed(40.millis, Input(1, "A")) ++
        Observable.evalDelayed(40.millis, Input(1, "B")) ++
        Observable.evalDelayed(20.millis, Input(1, "C")) ++
        Observable.evalDelayed(170.millis, Input(1, "D")) ++
        Observable.evalDelayed(400.millis, Input(1, "E"))

    val sourceB =
      Observable.eval(Input(2, "A")) ++
        Observable.evalDelayed(200.millis, Input(2, "B")) ++
        Observable.evalDelayed(150.millis, Input(2, "C")) ++
        Observable.evalDelayed(150.millis, Input(2, "D"))

    val source = Observable(sourceA, sourceB).merge

    var currentBatch = Seq.empty[Input]
    val f            = BufferingAndThrottling.ex3(source).foreach(currentBatch = _)

    s.tick(50.millis)
    assertEquals(currentBatch, List())

    s.tick(100.millis)
    assertEquals(currentBatch, List(Input(2, "A"), Input(1, "A")))

    s.tick(150.millis)
    assertEquals(currentBatch, List(Input(2, "B"), Input(1, "D")))

    s.tick(150.millis)
    assertEquals(currentBatch, List(Input(2, "C")))

    s.tick(150.millis)
    assertEquals(currentBatch, List(Input(2, "D")))

    s.tick(150.millis)
    assertEquals(currentBatch, List(Input(1, "E")))
  }
}
