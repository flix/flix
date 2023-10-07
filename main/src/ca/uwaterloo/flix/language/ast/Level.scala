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

import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Indicates a nesting depth.
  *
  * Each time evaluation enters a region, the level increases by one.
  * Currently, we only increase the nesting depth for regions (the scope expression).
  */

object Level {

  /**
    * The least-deep level.
    */
  val Top: Level = Level(0)

  /**
    * The default level, for when the level is not relevant.
    */
  val Default: Level = Level(0)

  /**
    * Sets the levels of all the type variables in the types to the variables' minimum level.
    *
    * For example, given
    *
    * {{{
    * tpe1 = a@2 + b@2
    * tpe2 = c@4 + d! + Pure
    * }}}
    *
    * the variables' levels are modified to
    *
    * {{{
    * a@2, b@2, c@2
    * }}}
    *
    * and the resulting types are
    *
    * {{{
    * tpe1 = a@2 + b@2
    * tpe2 = c@2 + d! + Pure
    * }}}
    *
    * Note that this modifies the variables' levels globally.
    */
  def equalize(tpe1: Type, tpe2: Type, renv: RigidityEnv): Unit = {
    val tvars = (tpe1.typeVars ++ tpe2.typeVars).filter(tv => renv.isFlexible(tv.sym))
    val levelOpt = tvars.map(_.sym.level).minOption
    levelOpt match {
      case Some(level) => tvars.foreach(_.sym.level = level)
      case None => ()
    }
  }
}

case class Level(i: Int) extends Ordered[Level] {
  if (i < 0) {
    throw InternalCompilerException(s"Unexpected negative level: $i", SourceLocation.Unknown)
  }

  override def compare(that: Level): Int = this.i.compare(that.i)

  def incr: Level = Level(i + 1)

  def decr: Level = Level(i - 1)
}

