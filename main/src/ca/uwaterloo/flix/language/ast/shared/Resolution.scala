/*
 * Copyright 2024 Holger Chenhao Gao
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{NamedAst, ResolvedAst, Symbol}
import ca.uwaterloo.flix.util.collection.Nel

/**
 * Result of a name resolution.
 */
sealed trait Resolution

object Resolution {
  case class Declaration(decl: NamedAst.Declaration) extends Resolution

  case class JavaClass(clazz: Class[?]) extends Resolution

  case class Var(sym: Symbol.VarSym) extends Resolution

  case class LocalDef(ann: Annotations, sym: Symbol.VarSym, fparams: Nel[ResolvedAst.FormalParam]) extends Resolution

  case class TypeVar(sym: Symbol.UnkindedTypeVarSym) extends Resolution

  case class Region(sym: Symbol.RegionSym) extends Resolution
}
