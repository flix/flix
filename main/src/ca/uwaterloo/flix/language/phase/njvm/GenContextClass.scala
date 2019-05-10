/*
 * Copyright 2019 Miguel Fialho
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
package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.{JvmName, NamespaceInfo, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.classes.{Context, RecordEmpty}

/**
  * Generates bytecode for the empty record class.
  */
object GenContextClass extends MnemonicsGenerator {

  /**
    * Method should receive a Map of all the generated classes so far. It should generate all the new classes
    * and return an updated map with the new generated classes.
    *
    * @param map of all the generated classes so far.
    * @param types  set of Monotypes this will be used to generate certain classes such as Enum.
    * @return update map with new generated classes
    */
  def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo], ns: Set[NamespaceInfo])(implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass] = {
    map + new Context(ns).getClassMapping
  }

}
