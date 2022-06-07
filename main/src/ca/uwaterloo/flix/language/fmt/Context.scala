/*
 * Copyright 2022 Paul Butcher
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
package ca.uwaterloo.flix.language.fmt

import scala.collection.mutable

trait Context {
  def getReadableAlias(id: Int): Int
}

class ReadableContext extends Context {
  private val tempIds = mutable.Map[Int, Int]()
  private var nextId = 0

  def getReadableAlias(id: Int): Int =
    tempIds.getOrElseUpdate(id, {nextId += 1; nextId})
}

class RawContext extends Context {
  def getReadableAlias(id: Int) = id
}
