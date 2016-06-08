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
  * A Java functional interface for JVM methods that are invokable by Flix.
  */
@FunctionalInterface
trait Invokable {

  /**
    * Invokes the method with the given Flix arguments `args`.
    *
    * The method *must* return a Flix value. Returning `null` corresponds to the `None` object of the `Opt` type.
    *
    * The method *must not* throw any exception.
    *
    * The arguments array `args` is guaranteed never to be `null`.
    */
  def apply(args: Array[IValue]): IValue

}
