/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.language.ast.{Symbol, MonoType}

/**
  * Meta information about a tag.
  */
// TODO: Magnus: What components are needed here?
// TODO: Magnus: Check the equality method.
case class TagInfo(sym: Symbol.EnumSym, tag: String, tparams: List[MonoType], enumType: MonoType, tagType: MonoType) {
  /**
    * Returns the hash code of `this` tag info.
    */
  override def hashCode(): Int = 7 * sym.hashCode + 11 * tag.hashCode

  /**
    * Returns `true` if the given `obj` is the same enum and tag as this tag info.
    */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: TagInfo => this.sym == that.sym && this.tag == that.tag && this.tparams == that.tparams
    case _ => false
  }
}