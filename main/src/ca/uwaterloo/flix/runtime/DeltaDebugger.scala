/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{MatchException, RuleException}

// TODO: Rename to minimizer?
object DeltaDebugger {

  /**
    * Returns `true` iff the two given exceptions `ex1` and `ex2` are the same.
    *
    * @param ex1 the 1st exception.
    * @param ex2 the 2nd exception.
    * @return `true` iff `ex1` is equal to `ex2`.
    */
  def sameException(ex1: RuntimeException, ex2: RuntimeException): Boolean = (ex1, ex2) match {
    case (MatchException(msg1, loc1), MatchException(msg2, loc2)) => msg1 == msg2 && loc1 == loc2
    case (RuleException(msg1, loc1), RuleException(msg2, loc2)) => msg1 == msg2 && loc1 == loc2
    case _ => ??? //ex1.getClass == ex2.getClass
  }


}
