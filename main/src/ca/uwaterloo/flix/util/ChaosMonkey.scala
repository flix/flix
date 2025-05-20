/*
 * Copyright 2025 Cade Lueker
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

import ca.uwaterloo.flix.api.Flix

import scala.util.Random

/**
  * Class for introducing randomness into Lists.
  */
object ChaosMonkey {
  /*
   * Static method for shuffling items in a container type.
   */
  def chaos[A](l: List[A])(implicit flix: Flix): List[A] = {
    // reorder the list when the Chaos Monkey is enabled.
    if (flix.options.xchaosMonkey)
      Random.shuffle(l)
    else
      l
  }
}
