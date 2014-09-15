package impl.runtime

import impl.logic.{Constraint, Predicate, Type, Value}
import syntax.Constraints.RichConstraint
import syntax.Predicates.RichPredicate
import syntax.Types.RichType
import util.ascii.{Align, AsciiTable}

import scala.collection.mutable

/**
 * A monitor of run-time performance.
 */
class Monitor {

  /**
   * A timer class.
   */
  class Timer() {
    // the total elapsed time.
    private var total: Long = 0
    // the number of times the timer has been resumed.
    private var hitcount: Int = 0
    // the time when the timer was resumed.
    private var time = System.nanoTime()

    /**
     * Measures the time spent executing the given function `f`.
     */
    def measure[A](f: => A): A = {
      resume()
      val r = f
      pause()
      r
    }

    /**
     * Resumes the timer (i.e. starts measuring elapsed time).
     */
    def resume(): Unit = {
      hitcount += 1
      time = System.nanoTime()
    }

    /**
     * Pauses the timer (i.e. stops measuring elapsed time).
     */
    def pause(): Unit = {
      total += (System.nanoTime() - time)
    }

    def count: Int = hitcount

    def ms: Int = ((total / 1000L) / 1000L).toInt
  }

  private val _init = new Timer()

  private val _fixpoint = new Timer()
  
  private val constraints = mutable.Map.empty[Constraint, Timer]

  private val predicates = mutable.Map.empty[Predicate, Timer]

  private val lessThanEqual = mutable.Map.empty[Type, Timer]

  private val leastUpperBound = mutable.Map.empty[Type, Timer]

  /**
   * Measures the time spent computing the initial facts.
   */
  def init(f: => Unit): Unit = _init.measure[Unit](f)

  /**
   * Measures the time spent computing the fixpoint.
   */
  def fixpoint(f: => Unit): Unit = _fixpoint.measure[Unit](f)

  /**
   * Measures the time evaluating a constraint.
   */
  def constraint[A](c: Constraint)(f: => A): A = {
    val t = constraints.getOrElseUpdate(c, new Timer())
    t.measure[A](f)
  }
  
  /**
   * Measures the time evaluating a predicate.
   */
  def predicate[A](p: Predicate)(f: => A): A = {
    val t = predicates.getOrElseUpdate(p, new Timer())
    t.measure[A](f)
  }

  /**
   * Measures the time spent computing less-than-equal operations for the given type `typ`.
   */
  def leq(typ: Type)(f: => Boolean): Boolean = {
    val t = lessThanEqual.getOrElseUpdate(typ, new Timer())
    t.measure[Boolean](f)
  }

  /**
   * Measures the time spent computing least-upper-bound operations for the given type `typ`.
   */
  def lub(typ: Type)(f: => Value): Value = {
    val t = leastUpperBound.getOrElseUpdate(typ, new Timer())
    t.measure[Value](f)
  }

  /**
   * Prints a summary of information to standard out.
   */
  def output(): Unit = {
    println(s"Init: ${_init.ms}ms, Iterate: ${_fixpoint.ms}ms, Total: ${_init.ms + _fixpoint.ms}ms")
    println()

    val cons = new AsciiTable()
      .mkCol("Constraint", Align.Left)
      .mkCol("Hitcount", Align.Right)
      .mkCol("Time (ms)", Align.Right)
      .mkRows(constraints.toList.sortBy(_._2.ms).reverse.map {
      case (c, timer) => List(c.fmt, timer.count, timer.ms + "ms")
    })
    println(cons.output)
    
    val preds = new AsciiTable()
      .mkCol("Predicates", Align.Left)
      .mkCol("Hitcount", Align.Right)
      .mkCol("Time (ms)", Align.Right)
      .mkRows(predicates.toList.sortBy(_._2.ms).reverse.map {
      case (p, timer) => List(p.fmt, timer.count, timer.ms + "ms")
    })
    println(preds.output)
    
    val leq = new AsciiTable()
      .mkCol("Less-Than-Equal (Type)", Align.Left)
      .mkCol("Hitcount", Align.Right)
      .mkCol("Time (ms)", Align.Right)
      .mkRows(lessThanEqual.toList.sortBy(_._2.ms).reverse.map {
      case (typ, timer) => List(typ.fmt, timer.count, timer.ms + "ms")
    })
    println(leq.output)

    val lub = new AsciiTable()
      .mkCol("Least-Upper-Bound (Type)", Align.Left)
      .mkCol("Hitcount", Align.Right)
      .mkCol("Time (ms)", Align.Right)
      .mkRows(leastUpperBound.toList.sortBy(_._2.ms).reverse.map {
      case (typ, timer) => List(typ.fmt, timer.count, timer.ms + "ms")
    })
    println(lub.output)


  }

}
