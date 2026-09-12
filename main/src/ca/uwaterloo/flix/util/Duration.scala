/*
 * Copyright 2017 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.util

object Duration {

  /**
    * Returns the minimal duration of `xs`.
    */
  def min(xs: Iterable[Duration]): Duration = xs.min

  /**
    * Returns the maximum duration of `xs`.
    */
  def max(xs: Iterable[Duration]): Duration = xs.max

  /**
    * Returns the average duration of `xs`.
    */
  def avg(xs: Iterable[Duration]): Duration = {
    val l = xs.toList
    new Duration(l.map(_.d).sum / l.length)
  }

  implicit val ordering: Ordering[Duration] = (x: Duration, y: Duration) => (x.d - y.d).asInstanceOf[Int]

}

/**
  * A simple class to format a time duration.
  *
  * @param d represents the duration in nanoseconds.
  */
case class Duration(d: Long) {

  /**
    * Returns the elapsed time in nanoseconds.
    */
  def nanoseconds: Double = d.toDouble

  /**
    * Returns the elapsed time in microseconds.
    */
  def microseconds: Double = nanoseconds / 1000.0

  /**
    * Returns the elapsed time in milliseconds.
    */
  def milliseconds: Double = microseconds / 1000.0

  /**
    * Returns the elapsed time in seconds.
    */
  def seconds: Double = milliseconds / 1000.0

  /**
    * Returns a human readable string of the elapsed time.
    */
  def fmt: String = {
    val OneMicroSecond = 1000L
    val OneMilliSecond = 1000L * OneMicroSecond
    val OneSecond = 1000L * OneMilliSecond

    if (d < OneMicroSecond)
      fmtNanoSeconds
    else if (d < OneMilliSecond)
      fmtMicroSeconds
    else if (d < OneSecond)
      fmtMilliSeconds
    else
      fmtSeconds
  }

  /**
    * Returns the elapsed time as a human readable string in nanoseconds.
    */
  def fmtNanoSeconds: String = f"$nanoseconds" + "ns"

  /**
    * Returns the elapsed time as a human readable string in microseconds.
    */
  def fmtMicroSeconds: String = f"$microseconds%.1f" + "us"

  /**
    * Returns the elapsed time as a human readable string in milliseconds.
    */
  def fmtMilliSeconds: String = f"$milliseconds%.1f" + "ms"

  /**
    * Returns the elapsed time as a human readable string in seconds.
    */
  def fmtSeconds: String = f"$seconds%.1f" + "s"

}
