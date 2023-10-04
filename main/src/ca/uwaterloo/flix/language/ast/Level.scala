/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.ast

/**
  * Indicates a nesting depth.
  *
  * Each time evaluation enters a region, level increases by one.
  */
case class Level(i: Int) extends AnyVal with Ordered[Level] {
  override def compare(that: Level): Int = this.i.compare(that.i)

  def incr: Level = Level(i + 1)

  def decr: Level = Level(i - 1)
}

object Level {

  /**
    * The highest level.
    */
  def Top: Level = Level(0)
}

