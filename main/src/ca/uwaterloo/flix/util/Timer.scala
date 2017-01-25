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

class Timer[T](f: => T) {

  private val OneMicroSecond = 1000L
  private val OneMiliSecond = 1000L * OneMicroSecond
  private val OneSecond = 1000L * OneMiliSecond

  private val b = System.nanoTime()
  private val r = f
  private val e = System.nanoTime()
  private val d = e - b

  def getResult: T = r

  def format: String = {
    if (d < OneMicroSecond)
      nanoseconds
    else if (d < OneMiliSecond)
      microseconds
    else if (d < OneSecond)
      miliseconds
    else
      seconds
  }

  def nanoseconds: String = f"$d ns."

  def microseconds: String = f"${d.toDouble / 1000.0}%.1f us."

  def miliseconds: String = f"${d.toDouble / 1000000.0}%.1f ms."

  def seconds: String = f"${d.toDouble / 1000000000.0}%.1f s."

}