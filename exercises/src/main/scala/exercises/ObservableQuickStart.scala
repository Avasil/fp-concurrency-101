package exercises

import exercises.ObservableQuickStart.BufferingAndThrottling.Input
import monix.catnap.ConcurrentQueue
import monix.eval.Task
import monix.execution.exceptions.DummyException
import monix.reactive.Observable

import scala.concurrent.duration._

/**
  * Observable docs (parts might be outdated: https://monix.io/docs/3x/reactive/observable.html )
  * Observable API: https://monix.io/api/3.1/monix/reactive/Observable.html
  * A lot of ReactiveX docs apply: http://reactivex.io/documentation/observable.html
  *
  * Welcome to Observable part!
  *
  * In short, Monix Observable is a data type for modeling and processing asynchronous
  * and reactive streaming of events with non-blocking back-pressure.
  *
  * It is strongly inspired by ReactiveX.
  *
  * We aim for Rx consistent API (with more pure functions here and there) so if you are familiar with any RX implementation
  * you should notice a lot of similarities and hopefully be comfortable quickly.
  *
  * Observable API is purely functional except for `Subject` and relevant operators which
  * serve to share `Observable` between multiple subscribers.
  *
  * The difference between a Task[A] and Observable[A] is that the latter supports streaming so instead of being
  * able to produce 0 or 1 value of type A, it can produce possibly infinite number of values.
  *
  * Observable API is quite big and there are a lot of things to talk about so this set of
  * exercises will only focus on essentials that are necessary to go through the main part of the workshop.
  */
object ObservableQuickStart extends App {

  /** `Observable` is lazily evaluated and no effects are executed until it is subscribed, i.e.:
    *
    * {{{
    *   val source = Observable.eval { println("effect!") }
    * }}}
    *
    * will only build a reusable piece.
    *
    * Once we subscribe, we can see the effect:
    *
    * {{{
    *   val c: Cancelable = source.subscribe()
    *   // effect! will be printed once
    * }}}
    *
    * It is also possible (and recommended) to subscribe to `Observable` by converting it to `Task`.
    * It can be achieved with one of the methods suffixed with "L".
    *
    * Some examples:
    * - `def completedL: Task[Unit]` will return a `Task` which will consume entire `Observable` and run until it finishes.
    * - `def toListL: Task[List[A]]` will return a `Task` which will produce a list with all elements that reached downstream
    * - `def firstL: Task[A]` will return a `Task` which will take the first element of `Observable` and finish it
    *
    * `Observable` is cancelable and if we convert it to `Task` and `cancel` that `Task` then the cancellation signal
    * will be propagated to the stream.
    *
    * Other way of receiving a `Task` is `Observable#consumeWith` which accepts a `Consumer`.
    * You can find predefined ones in `Consumer` companion object.
    *
    * For instance we can do:
    *
    * `source.consumeWith(Consumer.foreachTask(f))`
    *
    * instead of:
    *
    * `source.mapEval(f).completedL`
    *
    * to get rid of redundant last step.
    * Having said that, it's mostly a matter of preference.
    *
    */
  object CreatingAndRunning {

    /** Take a tour around Observable's companion object: https://monix.io/api/3.1/monix/reactive/Observable$.html
      * And find a builder to create an Observable which will emit incremented Long every 5 seconds
      */
    def ex1: Observable[Long] = {
      Observable.intervalAtFixedRate(5.second)
    }

    /** Use a method of your choice to return a `Task`
      * which will multiply all the numbers in `source` Observable
      */
    def ex2(source: Observable[Long]): Task[Long] = {
      source.foldLeftL(1L)(_ * _)
    }

    /** There are couple of ways to create `Observable` from external source.
      * One of the more convenient ways is to repeatedly check queue for new messages.
      *
      * Try to use `Observable` to consume from provided `ConcurrentQueue` and transform
      * it into a `Task` with a sum of all elements until there was a number <= 0.
      */
    def ex3(queue: ConcurrentQueue[Task, Long]): Task[Long] = {
      Observable
        .repeatEvalF(queue.poll)
        .takeWhile(_ > 0L)
        .sumL
    }
  }

  object Transforming {

    /** `Observable` has a `flatMap` which can be used when we need to produce a new stream
      * for each element.
      *
      * The behavior of `flatMap` can be illustrated as follows:
      *
      * `Observable(1, 2, 3).flatMap(i => Observable(i, i)) <-> Observable(1, 1, 2, 2, 3, 3)`
      *
      * It processes sub-streams in sequence so if we were to do:
      *
      * `Observable(1, 2, 3).flatMap(i => Observable.repeat(i))`
      *
      * We would receive an infinite stream of `1` and never process `2` or `3`
      *
      * Note that it is different than Rx where `flatMap` processes sub-streams
      * concurrently. We chose to make exception here to be more consistent with behavior in Scala ecosystem.
      *
      * Exercise:
      * Write `filter` in terms of `flatMap`
      *
      */
    def ex1(source: Observable[Long], f: Long => Boolean): Observable[Long] = {
      source.flatMap { i =>
        if (f(i)) Observable.now(i)
        else Observable.empty
      }
    }

    /** `mergeMap` is similar to `flatMap` but it processes all streams concurrently, that is:
      *
      * {{{
      *   Observable(1, 2, 3)
      *     .mergeMap(i => Observable.timerRepeated(initialDelay = 0.second, period = i.second, i))
      * }}}
      *
      * will produce `Observable` which will send `1` each second, `2` every 2 seconds and `3` every 3 seconds.
      *
      * Exercise:
      *
      * Write an `Observable` which will start emitting next natural number every 2 seconds and for each emitted
      * number it will keep emitting it for 4 seconds in 1 second intervals. Example:
      *
      * 0 // at 1s
      * 0, 0 // at 2s
      * 0, 0, 1, 0 // at 3s
      * 0, 0, 1, 0, 1, 0 // at 4s
      * 0, 0, 1, 0, 1, 0, 1, 2 // at 5s
      * 0, 0, 1, 0, 1, 0, 1, 2, 1, 2 // at 6s
      * ...
      *
      * (numbers could be interleaved differently due to concurrency)
      */
    def ex2: Observable[Long] = {
      Observable
        .intervalAtFixedRate(2.second)
        .mergeMap(l => Observable.timerRepeated(0.second, 1.second, l).take(4))
    }

    /** We can also map with functions which return an effect type such as `Task`.
      *
      * There are few variants, e.g.
      * `mapEval` (processes tasks one-by-one), `mapParallelOrdered` (processes up to N tasks in parallel and returns results downstream in order),
      * "Callback"-like methods `doOnNext`, `doOnError` etc. and quite a few more!
      *
      * Exercise:
      * Use `mapEval` to write a method which will end `Observable` with an error
      * if there is a negative number.
      */
    def ex3(source: Observable[Int], error: DummyException): Observable[Int] = {
      source.mapEval { i =>
        if (i < 0) Task.raiseError(error)
        else Task.now(i)
      }
    }

    /** `Observable.groupBy` processes source concurrently and splits it into
      * sub-streams grouped by specified key.
      *
      * Since we receive `Observable[GroupedObservable[A]]` it is usually combined with
      * `flatMap`, `mergeMap` or `switchMap`.
      *
      * For instance, let's say we want to write to the database concurrently but we also
      * need to preserve an order within some partitionKey.
      *
      * We can use `groupBy(_.parititionKey)` to have ordered sub-streams and then `mergeMap`
      * to process different sub-streams in parallel.
      *
      * Exercise:
      * Implement something similar to the operation described above.
      * Use `playerId` as partition key and use `insert` on each sub-stream concurrently
      *
      */
    def ex4(source: Observable[Input], insert: Input => Task[Unit]): Observable[Unit] = {
      source
        .groupBy(_.playerId)
        .mergeMap(_.mapEval(insert))
    }
  }

  /** One of the strengths of Monix Observable is a great control over time-based processing and back-pressure of the stream.
    * The back-pressure is supported out of the box but Observable also provide a variety of buffering and throttling
    * operators that can be handy in many situations.
    *
    * In this section, we will take a brief look at some of them.
    */
  object BufferingAndThrottling {

    /** Buffering allows us to work with batches of elements instead of processing them one-by-one.
      * Monix supports different kinds of buffering.
      *
      * We can simply buffer elements for a period of time and then emit the buffer with methods like `bufferTimed` and `bufferTimedAndCounted`.
      *
      * `bufferIntrospective` will buffer elements only if downstream is too slow and back-pressure upstream when needed.
      *
      * Other methods such as `bufferTimedWithPressure` and `bufferWithSelector` allow us to implement more sophisticated buffering logic.
      *
      * Exercise:
      *
      * Modify `Transforming.ex4` to buffer inputs from separate `playerId` and execute `processMany` in these batches.
      * Create a new buffer every 1 second.
      */
    def ex1(source: Observable[Input], processMany: Seq[Input] => Task[Unit]): Observable[Unit] = {
      source
        .groupBy(_.playerId)
        .mergeMap(_.bufferTimed(1.second))
        .mapEval(processMany)
    }

    /** There are at least two ways to throttle (limit rate) the stream.
      *
      * One approach is to drop excess elements which is useful in cases such as limiting user input in a given window
      * where it is perfectly fine to drop events.
      *
      * The other approach is to buffer elements and/or back-pressure the source to only allow `n` elements per `period` seconds.
      * We won't lose any elements this way but we will be slower to respond and not every source is happy can be back-pressured.
      *
      * Since the purpose of the workshop is to implement a computer game (which deals with player inputs) then
      * we will explore the former approach. Relevant methods are:
      * - `throttleFirst`
      * - `throttleLast`
      * - `debounce`
      * - `sampleBy` (for fancy use cases)
      *
      * Take a look at those and write a method and choose one to implement a method
      * which will execute the first `Input` in 1 second windows.
      */
    def ex2(source: Observable[Input], effect: Input => Task[Unit]): Observable[Input] = {
      source
        .throttleFirst(1.second)
        .doOnNext(effect)
    }

    /** The last exercise combines newly acquired skills and will be very helpful
      * in one of the tasks on server-side of the game.
      *
      * Transform `source` in such a way that it will gather the earliest input per `playerId` in 150 milliseconds windows
      *
      * Example:
      *
      *   Input(1, "A") - at 10 ms
      *   Input(1, "B") - at 100 ms
      *   Input(2, "A") - at 160 ms
      *   Input(1, "C") - at 200 ms
      *   ... silence
      *
      *   will emit:
      *   - Seq(Input(1, "A"), Input(2, "A") at 150 ms
      *   - Seq(Input(1, "C")) at 300 ms
      *
      * TIP: `Transforming.ex4` can be a good starting point
      */
    case class Input(playerId: Int, payload: String)

    def ex3(source: Observable[Input]): Observable[Seq[Input]] = {
      source
        .groupBy(_.playerId)
        .mergeMap(_.throttleFirst(150.millis))
        .bufferTimed(150.millis)
    }
  }
}
