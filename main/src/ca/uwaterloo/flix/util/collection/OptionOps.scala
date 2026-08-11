/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.util.collection

/**
  * Operations on options.
  */
object OptionOps {

  /**
    * Applies `f` to the value of `opt`, returning `opt` itself if `f`
    * returns a reference-equal (`eq`) value.
    *
    * Callers can detect "nothing changed" with a single reference equality check.
    */
  def mapWithReuse[T <: AnyRef](opt: Option[T])(f: T => T): Option[T] = opt match {
    case Some(x) =>
      val y = f(x)
      if (y eq x) opt else Some(y)
    case None => opt
  }
}
