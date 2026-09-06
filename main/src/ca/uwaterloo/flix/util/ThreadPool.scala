/*
 * Copyright 2026 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.api.CompilerConstants
import com.sun.management.HotSpotDiagnosticMXBean

import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{LinkedBlockingQueue, ThreadFactory, ThreadPoolExecutor, TimeUnit}

object ThreadPool {

  /**
    * The stack size (in bytes) requested for each worker thread: the larger of
    * [[CompilerConstants.ThreadStackSize]] and the JVM's default thread stack size.
    *
    * An explicit per-thread stack size overrides the JVM default rather than adding to it, so
    * without this maximum the workers would end up with *smaller* stacks than every other thread
    * whenever the JVM is run with a larger `-Xss`. Taking the maximum guarantees that the workers
    * are never worse off than the default.
    */
  private val WorkerStackSize: Long = math.max(CompilerConstants.ThreadStackSize, jvmDefaultStackSize())

  /**
    * Returns the JVM's default thread stack size in bytes, as set by `-Xss` or
    * `-XX:ThreadStackSize`, or `0` if it cannot be determined (e.g. on a non-HotSpot JVM or a
    * runtime image without the `jdk.management` module).
    */
  private def jvmDefaultStackSize(): Long = try {
    val bean = ManagementFactory.getPlatformMXBean(classOf[HotSpotDiagnosticMXBean])
    // HotSpot tracks `ThreadStackSize` in kilobytes.
    val kb = bean.getVMOption("ThreadStackSize").getValue.toLong
    kb * 1024L
  } catch {
    case _: Exception => 0L
    case _: LinkageError => 0L
  }

  /**
    * A [[ThreadFactory]] that creates daemon threads named `flix-worker-N` with a stack of
    * [[WorkerStackSize]] bytes.
    */
  private class WorkerFactory extends ThreadFactory {
    private val counter = new AtomicInteger(1)

    override def newThread(r: Runnable): Thread = {
      val t = new Thread(null, r, s"flix-worker-${counter.getAndIncrement()}", WorkerStackSize)
      t.setDaemon(true)
      t
    }
  }

}

/**
  * The thread pool used by the compiler: a fixed-size pool of `threads` worker threads, each
  * created with a stack of at least [[CompilerConstants.ThreadStackSize]] bytes.
  *
  * We use a plain [[ThreadPoolExecutor]] rather than a `ForkJoinPool` because the latter offers
  * no way to control the stack size of its worker threads: `ForkJoinWorkerThread` always requests
  * the JVM default. Here we supply our own [[ThreadFactory]] which requests a larger stack for
  * every worker.
  *
  * Idle workers exit after [[CompilerConstants.ThreadKeepAliveSeconds]] so that a pool which is
  * never shut down (e.g. because a compilation crashed) does not pin its threads and their stacks
  * forever.
  */
class ThreadPool(threads: Int) extends ThreadPoolExecutor(
  threads, threads,
  CompilerConstants.ThreadKeepAliveSeconds, TimeUnit.SECONDS,
  new LinkedBlockingQueue[Runnable](),
  new ThreadPool.WorkerFactory
) {
  allowCoreThreadTimeOut(true)
}
