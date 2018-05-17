/*
 * Copyright 2017 Magnus Madsen
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

/**
  * A simple class to measure the execution time of a function.
  */
class Timer[T](f: => T) {

  // Start the clock.
  private val b = System.nanoTime()

  // Evaluate the function.
  private val r = f

  // Stop the clock.
  private val e = System.nanoTime()

  // Compute the duration.
  private val d = e - b

  /**
    * Returns the result computed by the function `f`.
    */
  def getResult: T = r

  /**
    * Returns the elapsed time.
    */
  def getDuration: Long = d

  /**
    * Returns a duration formatter.
    */
  def getFormatter: DurationFormatter = new DurationFormatter(d)

}
