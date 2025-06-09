/*
 * Copyright 2022 Paul Butcher, Lukas Rønn, Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.api.lsp.provider.completion

/**
  * Represents the priority of a completion.
  *
  * A priority consists of an (A) absolute priority (e.g., `High` or `Low`) and (B) a relative priority (e.g., `High(5)`).
  */
sealed trait Priority {
  /**
    * Returns the relative priority of `this` priority.
    *
    * A relative priority may be negative.
    */
  def relative: Int
}

object Priority {
  case class Highest(relative: Int) extends Priority

  case class Higher(relative: Int) extends Priority

  case class High(relative: Int) extends Priority

  case class Medium(relative: Int) extends Priority

  case class Low(relative: Int) extends Priority

  case class Lower(relative: Int) extends Priority

  case class Lowest(relative: Int) extends Priority

  /**
    * Returns the given string `l` prefixed with the absolute and relative priority.
    *
    * The idea is to return a string whose lexicographic ordering matches the
    * implicit ordering of the absolute and relative priority plus the label.
    */
  def toSortText(p: Priority, l: String): String = {
    val a = p match {
      case Highest(_) => 1
      case Higher(_) => 2
      case High(_) => 3
      case Medium(_) => 4
      case Low(_) => 5
      case Lower(_) => 6
      case Lowest(_) => 7
    }

    val r = if (p.relative < 0) "0-" else "1-" + p.relative.abs.toString.padTo(4, '0')
    s"$a-$r-$l"
  }

}
