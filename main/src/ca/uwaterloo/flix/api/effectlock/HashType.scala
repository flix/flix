/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.security.MessageDigest

class HashType {
  private val md = MessageDigest.getInstance("SHA-256")

  def treeHash(tpe0: Type): Array[Byte] = tpe0 match {
    case Type.Var(sym, _) => ???
    case Type.Cst(tc, _) =>
    case Type.Apply(tpe1, tpe2, _) => ???
    case Type.AssocType(SymUse.AssocTypeSymUse(sym, loc), arg, kind, _) => ???
    case Type.Alias(symUse, args, tpe, loc) => throw InternalCompilerException("Unexpected type alias", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("Unexpected Java type", loc)
  }

  private def hashTypeConstructor(tc0: TypeConstructor): Unit = ???

  private def hashKindedTypeVarSym(sym0: Symbol.KindedTypeVarSym): Unit = ???
}
