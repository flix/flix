/*
 * Copyright 2022 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.Symbol

/**
  * Represents a unit test.
  *
  * @param sym     the Flix def symbol.
  * @param runProp the property relevant to running the test.
  */
case class TestFn(sym: Symbol.DefnSym, runProp: TestFn.RunProperty)

object TestFn {

  /**
    * A property relevant to running the unit test.
    */
  sealed trait RunProperty

  object RunProperty {
    /**
      * Holds the function to execute for running the test.
      */
    case class Runnable(run: () => AnyRef) extends RunProperty

    /**
      * Indicates that the test should be skipped.
      */
    case object Skipped extends RunProperty

    /**
      * Indicates that the test is invalid.
      */
    case class Invalid(reason: String) extends RunProperty
  }
}
