package ca.uwaterloo.flix.runtime

import java.util.concurrent.Executors

/**
 * A class used to monitor the performance of the fixpoint solver.
 *
 * Usage of this class may incur additional solver overhead.
 */
class Monitor(solver: Solver) {

  /**
   * A scheduled thread pool used to periodically run the sampler.
   */
  val pool = Executors.newScheduledThreadPool(1)

  class Sampler extends Runnable {

    val e = System.nanoTime()

    case class DataSample(time: Long, facts: Int, worklist: Int, memory: Long)

    var telemetry = List.empty[DataSample]

    def takeSnapshot(): Unit = {
      val usedMemory = (Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()) / (1024 * 1024)
      telemetry ::= DataSample(System.nanoTime() - e, solver.dataStore.totalFacts, solver.worklist.size, usedMemory)
    }

    def run(): Unit = try {
      while (!Thread.currentThread().isInterrupted) {
        takeSnapshot()
        Thread.sleep(1000) // TODO: Need to use a schedyled thread poool
      }
    } catch {
      case e: InterruptedException =>
        takeSnapshot()
    }

  }

  /**
   * Start performance monitoring.
   *
   * A monitor can only be started and stopped once.
   */
  def start(): Unit = {

  }

  /**
   * Stop performance monitoring.
   *
   * A monitor can only be started and stopped once.
   */
  def stop(): Unit = {

  }

}