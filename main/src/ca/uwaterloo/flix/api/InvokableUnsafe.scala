/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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

package ca.uwaterloo.flix.api

/**
  * A Java functional interface for unsafe JVM methods that are invokable by Flix.
  */
@FunctionalInterface
trait InvokableUnsafe {

  /**
    * Invokes the method with the given Flix arguments `args`.
    *
    * The arguments are passed using the internal Flix representation of values.
    *
    * Similarly the returned value must correspond to Flix' internal representation of values.
    */
  def apply(args: Array[AnyRef]): AnyRef

}
