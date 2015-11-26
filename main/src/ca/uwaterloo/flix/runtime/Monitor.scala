package ca.uwaterloo.flix.runtime

import java.util.concurrent.{TimeUnit, Executors}

/**
 * Companion object for the [[Monitor]] class.
 */
object Monitor {

  /**
   * A class that represents a single sample of performance data.
   *
   * @param time the time when the sample was taken.
   * @param queue the  total size of the queue.
   * @param facts the total number of facts.
   * @param memory the amount memory used.
   */
  case class Sample(time: Long, queue: Int, facts: Int, memory: Long)

}

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

  /**
   * The singleton sampler instance.
   */
  val sampler = new Sampler

  /**
   * A class used to performance period sampling of the performance.
   */
  class Sampler extends Runnable {
    /**
     * Records the time when the sampler was started. 
     */
    val zero = System.nanoTime()

    /**
     * Records a sequence of samples.
     */
    @volatile
    var samples = List.empty[Monitor.Sample]

    def run(): Unit = {
      val elapsedTime = System.nanoTime() - zero
      val queueSize = solver.worklist.size
      val totalFacts = solver.dataStore.totalFacts
      val totalMemoryInBytes = Runtime.getRuntime.totalMemory()
      val totalFreeMemoryInBytes = Runtime.getRuntime.freeMemory()
      val usedMemoryInMegaBytes = (totalMemoryInBytes - totalFreeMemoryInBytes) / (1024 * 1024)
      samples ::= Monitor.Sample(elapsedTime, queueSize, totalFacts, usedMemoryInMegaBytes)
    }

  }

  /**
   * Returns the current telemetry.
   */
  def getTelemetry: List[Monitor.Sample] = sampler.samples

  /**
   * Start performance monitoring.
   *
   * A monitor can only be started and stopped once.
   */
  def start(): Unit = {
    pool.scheduleAtFixedRate(sampler, 0, 1, TimeUnit.SECONDS)
  }

  /**
   * Stop performance monitoring.
   *
   * A monitor can only be started and stopped once.
   */
  def stop(): Unit = {
    sampler.run()
    pool.shutdownNow()
  }

}