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
import ca.uwaterloo.flix.language.phase.jvm.{ClosureInfo, JvmName, NamespaceInfo, TagInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.classes.RecordEmpty

/**
  * Generates bytecode for the empty record class.
  */
object GenRecordEmpty extends MnemonicsGenerator {

  // TODO: Miguel: Is it really worth it to have these small 2line classes? Or at least we should the move them into Mnemonics.
  def gen(map: Map[JvmName, MnemonicsClass], types: Set[MonoType], tags: Set[TagInfo],
          ns: Set[NamespaceInfo], closures: Set[ClosureInfo])
         (implicit root: Root, flix: Flix): Map[JvmName, MnemonicsClass]  = {
    map + new RecordEmpty(map).getClassMapping
  }

}
