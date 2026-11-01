/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.perf

import ca.uwaterloo.flix.tools.compilertop.Ansi
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

/**
  * Receives progress events from `Xperf` and shows them to the user.
  *
  * All methods are called from the benchmark thread, in this order per run: [[runStarted]],
  * then [[runFinished]]. [[experimentFinished]] follows the last run of an experiment. Exactly
  * one of [[stop]] and [[fail]] ends the session.
  */
trait PerfMonitor {

  /** A run is about to start. `iteration` is one-based, or zero for a discarded warm-up run. */
  def runStarted(e: Experiment, iteration: Int): Unit

  /** The run started by the matching [[runStarted]] has completed. */
  def runFinished(e: Experiment, iteration: Int, run: Run): Unit

  /** All runs of the experiment have completed. */
  def experimentFinished(e: Experiment): Unit

  /** Tears down the display because an error is about to be printed and the benchmark will exit. */
  def fail(): Unit

  /** The benchmark has completed. Shows the final state and leaves it on screen. */
  def stop(): Unit

}

object PerfMonitor {

  /**
    * Creates a monitor: a live dashboard when attached to a console, otherwise a log that prints
    * one line per completed run and the final frame at the end.
    *
    * @param n           the number of measured runs per experiment.
    * @param experiments the experiments in execution order, each with its number of threads.
    */
  def create(n: Int, experiments: List[(Experiment, Int)]): PerfMonitor = {
    val states = experiments.map { case (e, threads) => ExperimentState(e, threads, Vector.empty, None, None) }.toVector
    val initial = PerfState(n, states, measuring = false, System.nanoTime())
    if (System.console() == null) new LogMonitor(initial) else new LiveMonitor(initial)
  }

  /**
    * Tracks the benchmark progress as a [[PerfState]] so that every monitor renders the same frame.
    */
  private abstract class StateMonitor(initial: PerfState) extends PerfMonitor {

    /** The benchmark progress. Written by the benchmark thread, read by whichever thread renders. */
    @volatile private var state: PerfState = initial

    def runStarted(e: Experiment, iteration: Int): Unit =
      state = state.withStarted(e, System.nanoTime()).copy(measuring = iteration >= 1)

    def runFinished(e: Experiment, iteration: Int, run: Run): Unit = {
      val s = if (iteration >= 1) state.withRun(e, run) else state
      state = s.copy(measuring = false)
    }

    def experimentFinished(e: Experiment): Unit =
      state = state.withEnded(e, System.nanoTime())

    /** Takes a snapshot for rendering: the benchmark progress stamped with the current time. */
    protected def snapshot(): PerfState = state.copy(nowNanos = System.nanoTime())

  }

  /**
    * Prints one plain line per completed run and, when the benchmark completes, the final frame
    * without styling. Used when stdout is not a console.
    */
  private final class LogMonitor(initial: PerfState) extends StateMonitor(initial) {

    override def runFinished(e: Experiment, iteration: Int, run: Run): Unit = {
      super.runFinished(e, iteration, run)
      println(PerfRenderer.renderLogLine(e, iteration, initial.n, run))
    }

    def fail(): Unit = ()

    def stop(): Unit = {
      println()
      print(PerfRenderer.render(snapshot(), PerfRenderer.DefaultCols, ansi = false))
      System.out.flush()
    }

  }

  /**
    * Redraws a dashboard in place several times per second.
    *
    * The frame is drawn below whatever was printed before it. Each redraw moves the cursor up
    * over the previous frame and overwrites it line by line, so the terminal's scrollback is
    * untouched and the final frame stays on screen after [[stop]].
    */
  private final class LiveMonitor(initial: PerfState) extends StateMonitor(initial) {

    /** The delay between frames. */
    private val FrameDelayMillis: Long = 100L

    /** Guards terminal writes and the fields below. */
    private val lock = new ReentrantLock()

    /** The number of lines the previous frame occupies, or zero before the first frame. */
    private var linesDrawn: Int = 0

    /** Whether the session has ended. No frames are drawn afterwards. */
    private var stopped: Boolean = false

    /** A JLine terminal used only to query the screen size. May be `null` if JLine fails. */
    private val terminal: Terminal = {
      java.util.logging.Logger.getLogger("org.jline").setLevel(java.util.logging.Level.OFF)
      try TerminalBuilder.builder().system(true).build()
      catch {
        case _: Throwable => null
      }
    }

    /** The render thread. */
    private val animator: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor((r: Runnable) => {
      val thread = new Thread(r, "flix-perf-monitor")
      thread.setDaemon(true)
      thread
    })

    animator.scheduleAtFixedRate(() => frame(), 0L, FrameDelayMillis, TimeUnit.MILLISECONDS)

    def fail(): Unit = shutdown(drawFinal = false)

    def stop(): Unit = shutdown(drawFinal = true)

    /**
      * Stops the render thread and, if `drawFinal` is set, draws the finished state once more.
      * Waits for an in-flight frame so the final frame is never interleaved with it.
      */
    private def shutdown(drawFinal: Boolean): Unit = {
      animator.shutdownNow()
      lock.lock()
      try {
        if (stopped) return
        stopped = true
        if (drawFinal) draw(snapshot())
      } finally {
        lock.unlock()
      }
      if (terminal != null) {
        try terminal.close()
        catch {
          case _: Throwable => ()
        }
      }
    }

    /** Render-thread body: draws one frame unless the session has ended. */
    private def frame(): Unit = {
      lock.lock()
      try {
        if (!stopped) draw(snapshot())
      } finally {
        lock.unlock()
      }
    }

    /**
      * Draws the frame over the previous one.
      *
      * Locking: must only be called while holding `lock`.
      */
    private def draw(s: PerfState): Unit = {
      val text = PerfRenderer.render(s, cols(), ansi = true)
      val lines = text.count(_ == '\n')
      val sb = new StringBuilder
      sb.append(Ansi.BeginSync)
      if (linesDrawn > 0) sb.append(s"${Ansi.ESC}[${linesDrawn}A")
      sb.append('\r')
      sb.append(text)
      // Clear anything below the frame in case it shrank.
      sb.append(s"${Ansi.ESC}[J")
      sb.append(Ansi.EndSync)
      System.out.print(sb.toString)
      System.out.flush()
      linesDrawn = lines
    }

    /** The terminal width, or the renderer's default if unknown. */
    private def cols(): Int = {
      if (terminal == null) return PerfRenderer.DefaultCols
      val w = terminal.getWidth
      if (w > 0) w else PerfRenderer.DefaultCols
    }

  }

}
