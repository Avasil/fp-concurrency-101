package exercises

import cats.effect.concurrent.{Deferred, Ref, Semaphore}
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration.{FiniteDuration, _}

/**
  * Task docs (parts might be outdated: https://monix.io/docs/3x/eval/task.html )
  * Task API: https://monix.io/api/3.1/monix/eval/Task.html
  *
  * Great explanation of benefits of representing side-effects as pure values:
  * https://www.reddit.com/r/scala/comments/8ygjcq/can_someone_explain_to_me_the_benefits_of_io/e2jfp9b/
  */
object TaskQuickStart extends App {

  /** Task represents a specification for a possibly lazy or asynchronous computation.
    * Building a Task[A] is just a description of a computation which after running
    * will produce a value of type A, return with an exception or never return.
    *
    * We can use Task to turn side-effects into pure (referentially transparent)
    * values which can be reused or enriched with extra logic like retries or timeouts.
    */
  object CreatingAndRunning {

    /** Scheduler is responsible for executing (scheduling!) Tasks to run on a thread.
      * If you're familiar with ExecutionContext from Future - it is the equivalent
      * and it actually extends it so we can easily interop between Task and Future
      * and pass the same Scheduler to both.
      *
      * Task is lazy and unlike Future it does not need Scheduler for every operation.
      * It is only needed if we execute the Task or if we want to change the current Scheduler.
      */
    implicit val s: Scheduler = monix.execution.Scheduler.global

    /** One of the simplest way to construct a Task is to wrap a computation
      * with `Task.apply`, `Task.eval` or `Task.evalAsync`, i.e.
      *
      * `val task = Task { println("suspended") }`
      *
      * This way, nothing will happen until we actually run the task.
      *
      * There are multiple ways to run a Task, all of them have method names
      * prefixed with "run", that is "runToFuture", "runSyncUnsafe", "runAsyncAndForget", etc.
      *
      * If we run our task:
      *
      * `task.runSyncUnsafe`
      *
      * We will see "suspended" printed to the screen.
      *
      * There are also builders for using already computed values such as
      * `Task.now(a: A)` and `Task.raiseError(ex: Throwable)`.
      *
      * The former will lift a value to `Task` and the latter
      * will create a Task that returns with an error.
      *
      * Note that these constructors do not suspend the body and be careful of passing
      * anything else there because it will execute outside of Task context.
      */
    def ex1(): Unit = {
      var effectEval = 0
      var effectNow  = 0

      val suspendedTask: Task[Int]    = Task.eval { effectEval += 1; effectEval }
      val notSuspendedTask: Task[Int] = Task.now { effectNow += 1; effectNow }

      /** Task is a description of a computation (like a recipe or a function), so when executing it twice
        * we should always expect that it will run its effects twice.
        */
      suspendedTask.runSyncUnsafe()
      suspendedTask.runSyncUnsafe()
      println(s"effectEval: $effectEval")

      notSuspendedTask.runSyncUnsafe()
      notSuspendedTask.runSyncUnsafe()
      println(s"effectNow: $effectNow")
    }

    /** Callback-based asynchronous API can be quite common when
      * interacting with impure libraries and you will definitely
      * see it a lot in Scala.js
      *
      * It is possible to use callbacks to signal completion of a Task
      * with methods such as `Task.async` or `Task.create` which aggregates
      * all available variants.
      *
      * As an exercise, try to use `Task.async` to create a Task from `Sum`
      */
    class Sum {
      private[this] var i = 0

      var _onError: Throwable => Unit = null
      var _onComplete: Int => Unit    = null

      def onNext(elem: Int): Unit = i += elem

      def onError(e: Throwable): Unit = _onError(e)

      def onComplete(): Unit = _onComplete(i)
    }

    def ex2(sum: Sum): Task[Int] = {
      Task.async[Int] { cb =>
        sum._onError = cb.onError
        sum._onComplete = cb.onSuccess
      }
    }
  }

  /** `Task` can be combined with operators like `map` or `flatMap`.
    *  In purely functional applications, we aim to compose tasks over
    *  entire program and execute it only once in Main.
    *
    * {{{
    *   val hello = Task("Hello ")
    *   val world = Task("World!")
    *
    *   // Will return "Hello World!" after execution
    *   val sayHello: Task[String] = hello
    *     .flatMap(h => world.map(w => h + w))
    *
    *   // We could use `map2` instead
    *   val sayHello2: Task[String] = Task.map2(hello, world)(_ + _)
    * }}}
    *
    * Such operators will combine Task in sequence:
    *
    * {{{
    *   val ta = Task(println("A"))
    *   val tb = Task(println("B"))
    *   val tc = Task(println("C"))
    *
    *   // prints A, then B, then C after execution
    *   val t = ta.flatMap(_ => tb).flatMap(_ => tc)
    * }}}
    *
    * Other handy operator is `sequence` and `traverse` which can be used
    * to combine a list of tasks into one single task with all the results:
    *
    * {{{
    *   val listOfTasks: List[Task[Unit]] = List(ta, tb, tc)
    *
    *   // prints A, then B, then C after execution
    *   val task: Task[List[Unit]] = Task.sequence(listOfTasks)
    * }}}
    *
    * {{{
    *   val f: String => Task[Unit] = s => Task(println(s))
    *   val strings = List("A", "B", "C")
    *
    *   // prints A, then B, then C after execution
    *   val task: Task[List[Unit]] = Task.traverse(strings)(f)
    * }}}
    *
    */
  object Composing {

    /**
      * Write a Task which will produce a sum of `a` and `b`
      */
    def ex1(a: Task[Int], b: Task[Int]): Task[Int] = {
      Task.map2(a, b)(_ + _)
    }

    /**
      * Write a Task which will run `compute` for each number in the list
      * and then sum it
      */
    def ex2(numbers: List[Int], compute: Int => Task[Int]): Task[Int] = {
      Task.traverse(numbers)(n => compute(n)).map(_.sum)
    }
  }

  /** When `Task` fails with an error it short-circuits the computation:
    *
    * {{{
    *   val ta = Task(println("A"))
    *   val tb = Task.raiseError(DummyException("boom"))
    *   val tc = Task(println("C"))
    *
    *   // prints A, then throws DummyException
    *   val t = ta.flatMap(_ => tb).flatMap(_ => tc)
    * }}}
    *
    * To prevent this, we can handle the error with one of many available methods.
    * It's best if you search the API for a methods prefixed with `onError`.
    * Some of the methods: `onErrorHandle`, `onErrorHandleWith`, `attempt`, ...
    *
    * {{{
    *   val ta = Task(println("A"))
    *   val tb = Task.raiseError(DummyException("boom"))
    *   val tc = Task(println("C"))
    *
    *   // prints "A", then "B recovered", then "C"
    *   val t = ta
    *     .flatMap(_ => tb)
    *     .onErrorHandleWith(_ => Task(println("B recovered"))
    *     .flatMap(_ => tc)
    *     .onErrorHandleWith(_ => Task(println("C recovered"))
    * }}}
    *
    */
  object ErrorHandling {

    /** Write a method which handles all errors in a Task end exposes them as `Left` in `Either`.
      *
      * There is a function called `attempt` which does it but try to implement
      * it on your own
      */
    def ex1[A](task: Task[A]): Task[Either[Throwable, A]] = {
      task.map(a => Right(a)).onErrorHandle(e => Left(e))
    }

    /** Write a recursive function which will retry Task up to `maxRetries`
      * with exponential backoff between tries.
      *
      * Tip: Check `Task.sleep` or `Task#delayExecution` for delaying execution of a `Task`.
      *      Note that unlike `Thread.sleep`, `Task.sleep` is non-blocking!
      */
    def ex2[A](source: Task[A], maxRetries: Int, firstDelay: FiniteDuration): Task[A] = {
      source.onErrorHandleWith {
        case ex: Exception =>
          if (maxRetries > 0)
            ex2(source, maxRetries - 1, firstDelay * 2)
              .delayExecution(firstDelay)
          else
            Task.raiseError(ex)
      }
    }
  }

  /** One of the main use cases of `Task` is concurrency which is about processing tasks interleaved,
    * possibly on a different thread of execution.
    *
    * For example, imagine you are messaging with 10 people at the same time and need to alternate
    * between them to give your responses.
    *
    * `Task` is submitted for execution to `Scheduler`. Internally, Scheduler has a work queue and it
    * governs a pool of Threads. It schedules a task to run on the Thread.
    * Then Operating System is responsible to eventually run this Thread on CPU.
    *
    * Tasks are running on a given thread until they finish or "yield" control back to the Scheduler.
    * We usually describe it as "asynchronous boundaries".
    *
    * It will happen when calling `executeAsync`, `shift`, `sleep`, waiting on asynchronous data structures,
    * at the beginning of parallel operators and so on. Default `ExecutionModel` of Scheduler will also
    * introduce automatic asynchronous boundaries every N flatMaps to improve fairness.
    *
    * When async boundary happens, the Task will be rescheduled to continue execution in the future.
    * It is possible it will run on the same thread again, or perhaps continue on a different one.
    * Too many asynchronous boundaries can add unnecessary overhead due to context switches.
    */
  object Concurrency {

    /** If we run this code on a single threaded `Scheduler`, `otherTask` might not execute at all.
      * Can you explain why?
      *
      * Try to fix it so `Concurrency.ex1` test in `TaskQuickStartSuite` passes.
      * Then revert your fix and try to experiment with different ExecutionModels on Scheduler.
      */
    def ex1(otherTask: Task[Unit]): Task[Unit] = {
      def forever: Task[Unit] = Task.evalAsync(()).flatMap(_ => forever)

      Task.parMap2(forever, otherTask)((_, _) => ())
    }

    /** Task has a notion of default Scheduler - the one that was passed when executing it.
      *
      * All asynchronous boundaries will return the Task to the default Scheduler.
      * We can also override it with `executeOn`.
      *
      * There is rarely a need for many different Schedulers but many applications have at least two:
      * - "Main" Scheduler (e.g. `Scheduler.global` or `Scheduler.computation`) for most computations, CPU-bound in particular
      * - "Blocking" Scheduler (e.g. `Scheduler.io`) for computations which block threads
      *
      * The main Scheduler has usually bounded thread pool which help minimizing context switches which happen when
      * a new thread is taking CPU.
      *
      * The blocking Scheduler has often unbounded, cached thread pool.
      * If we have limited thread pool and block all available threads we might end up with a deadlock because
      * there is no thread left that could execute callbacks to unblock some of them.
      *
      * For the exercise, execute `blockingTask` on `blockingScheduler` and keep the rest of the computation
      * on a default one. Try to experiment with different methods, like `executeOn` or `shift(ec)` to see the difference!
      */
    def ex2(normalTask: Task[Unit], blockingTask: Task[Unit], blockingScheduler: Scheduler): Task[Unit] = {
      for {
        _ <- normalTask
        _ <- blockingTask.executeOn(blockingScheduler)
        _ <- normalTask
      } yield ()
    }
  }

  /** Parallelism is all about processing tasks at the same time to complete the work faster.
    *
    * Task provides many operators for parallel execution,
    * e.g. "parZip", "parMap", "gather" (soon to be aliased to "parSequence"), "wander" (soon to be aliased to "parTraverse"), "race", ...
    *
    * It is not guaranteed it will actually run in parallel.
    * It depends on the thread pool (Scheduler) and the amount of available CPU cores.
    *
    * Nonetheless, these operators can be very useful even on a single thread if we do concurrent I/O calls or to
    * interleave work which can give as an illusion of true parallelism.
    */
  object Parallelism {

    /** If we are executing multiple tasks concurrently, it is possible that one of them could fail.
      * The default behavior is that all other task will be canceled and the Task will return with
      * the first failure (other possible failures will be reported to Scheduler).
      *
      * What if we would like to ignore errors instead?
      * Fortunately, effect types like Monix Task make such modifications very easy!
      *
      * Try to process `tasks` in parallel in a way that would ignore the failures.
      */
    def ex1(tasks: List[Task[Unit]]): Task[List[Unit]] = {
      Task.wander(tasks)(_.onErrorHandle(_ => ()))
    }

    /** Now try to modify solution to `ex1` further to return `Task[(Int, Int)]`
      * where the tuple is (numberOfSuccesses, numberOfFailures).
      */
    def ex2(tasks: List[Task[Unit]]): Task[(Int, Int)] = {
      Task.wander(tasks)(_.attempt).map { list =>
        val (successes, failures) = list.partition(_.isRight)
        (successes.size, failures.size)
      }
    }
  }

  /** Monix Task supports cancellation.
    * When the Task is canceled, it becomes non-terminating (does not return result).
    * `Task#onCancelRaiseError` can be used to modify this behavior and raise an error instead.
    *
    * Note that cancellation is checked during asynchronous boundaries - if we try to cancel a long-running,
    * synchronous `Task` (e.g. equivalent of `Task.eval { while(true) {} }` then we will have to wait until it
    * finishes because there is no way of magically interrupting any arbitrary code on the JVM.
    *
    * The safest and the best way to cancel a Task is using the behavior encoded in parallel operators or `timeout`.
    * Other good way which is not pure is canceling `CancelableFuture` returned after `runToFuture`.
    *
    * We can also receive `cancel` handle for any task by using `Task#start` which will give us a `Fiber`.
    * Start is considered low-level and prefer aforementioned ways of cancellation if they are good enough for your use
    * case or if you know what are you doing. :)
    *
    * Let's use each method to solve the same simple task and get comfortable with the process
    */
  object Cancellation {

    /** Run both tasks concurrently and cancel the slower one but only if the first one finished with a success.
      * Use `Task.race`
      */
    def ex1[A, B](ta: Task[A], tb: Task[B]): Task[Unit] = {
      Task.race(ta.onErrorHandleWith(_ => Task.never), tb.onErrorHandleWith(_ => Task.never)).map(_ => ())
    }

    /** Run both tasks concurrently and cancel the slower one but only if the first one finished with a success.
      * Use `Task#runToFuture`
      */
    def ex2[A, B](ta: Task[A], tb: Task[B])(implicit s: Scheduler): Unit = {
      val fa = ta.runToFuture
      val fb = tb.runToFuture

      fa.map(_ => fb.cancel())
      fb.map(_ => fa.cancel())
      ()
    }

    /** Run both tasks concurrently and cancel the slower one but only if the first one finished with a success.
      * Use `Task#start`
      */
    def ex3[A, B](ta: Task[A], tb: Task[B]): Task[Unit] = {
      for {
        fiberA <- ta.start
        fiberB <- tb.start
        _      <- fiberA.join.flatMap(_ => fiberB.cancel).startAndForget
        _      <- fiberB.join.flatMap(_ => fiberA.cancel).startAndForget
      } yield ()
    }
  }

  /** If your use case demands some concurrency then it is likely you might want to
    * combine Task with other structures.
    *
    * You can find some in Cats-Effect itself:
    * - https://typelevel.org/cats-effect/concurrency/
    *
    * Monix-Catnap module:
    * - https://monix.io/api/3.1/monix/catnap/index.html
    *
    * And probably all over the place in random github repositories or gists. :')
    *
    * If you're wondering why all the constructors are suspended in `F[_]` (in our case Task) then
    * this talk contains a fantastic explanation: https://vimeo.com/294736344
    */
  object ConcurrencyPrimitives {

    /** `Ref`is probably the most common one you will encounter.
      *  If you are familiar with `AtomicReference` - this is purely functional equivalent.
      *
      *  Whenever you need thread-safe variable - `Ref` should do the job.
      * - https://typelevel.org/cats-effect/concurrency/ref.html
      *
      * Exercise:
      * Create `Ref[Task, Int]` initiated with 0 and then create two tasks updating it in parallel:
      *  a) One adding +1 every 2 seconds
      *  b) One multiplying *2 every 3 seconds
      *
      * As a result of the main Task return created `Ref` and updating `Task`.
      *
      * TIPs:
      * - You can use `Task.sleep` or `Task.delayExecution` to introduce a time delay in a `Task`.
      * - `loopForever` might be more convenient than manual recursion to create "infinite" `Task`
      */
    def ex1: Task[(Ref[Task, Int], Task[Unit])] = {
      for {
        ref <- Ref.of[Task, Int](0)
        update = Task
          .parZip2(
            ref.update(_ + 1).delayExecution(2.second).loopForever,
            ref.update(_ * 2).delayExecution(3.second).loopForever
          )
          .map(_ => ())
      } yield (ref, update)
    }

    /** Now try to use `Semaphore` to implement a naive implementation of `wanderN`
      * which only executes up to `n` tasks in parallel.
      *
      * https://typelevel.org/cats-effect/concurrency/semaphore.html
      */
    def ex2[A, B](n: Int, seq: List[A])(f: A => Task[B]): Task[List[B]] = {
      for {
        s       <- Semaphore[Task](n)
        results <- Task.wander(seq)(a => s.withPermit(f(a)))
      } yield results
    }

    /** Remember example with counting errors and successes?
      * Let's modify it further and create a method which will run
      * BOTH `tasks` and `otherTask` in parallel but it will cancel `otherTask`
      * only if `tasks` complete with at least 3 errors.
      *
      * TIP: Look into `Deferred` and `race` combinations
      * - https://typelevel.org/cats-effect/concurrency/deferred.html
      */
    def ex3[A](tasks: List[Task[A]], otherTask: Task[A]): Task[Unit] = {
      for {
        deferred <- Deferred[Task, Unit]
        _ <- Task
          .wander(tasks)(_.attempt)
          .flatMap(results => if (results.count(_.isRight) >= 3) deferred.complete(()) else Task.unit)
          .startAndForget
        _ <- Task.race(otherTask, deferred.get)
      } yield ()
    }
  }

  /** That's it, congratulations!
  * If you would like solve more concurrency puzzles then I recommend you to take a look at Oleg's blog:
  * - https://olegpy.com/cats-effect-exercises/
  * - https://olegpy.com/resource-exercises/
  */
}
