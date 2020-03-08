package exercises

import minitest.TestSuite
import monix.execution.schedulers.TestScheduler

abstract class BaseTestSuite extends TestSuite[TestScheduler] {

  def setup(): TestScheduler = TestScheduler()

  def tearDown(env: TestScheduler): Unit = {
    assert(env.state.tasks.isEmpty, "should not have tasks left to execute")
  }
}
