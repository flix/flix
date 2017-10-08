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

package ca.uwaterloo.flix.util.tc

/**
  * Type class for values that can be shown.
  */
trait Show[A] {
  /**
    * Returns a string representation of `a`.
    */
  def show(a: A): String
}

/**
  * Companion object of [[Show]].
  */
object Show {

  /**
    * Adds a `show` method to every instance of [[Show]].
    */
  implicit class ShowableSyntax[A: Show](a: A) {
    def show: String = implicitly[Show[A]].show(a)
  }

}
