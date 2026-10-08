/*
 * Copyright 2024 Holger Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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

  case class JavaClass(clazz: ca.uwaterloo.flix.language.ast.jvm.JavaClass) extends Resolution

  case class Var(sym: Symbol.VarSym) extends Resolution

  case class LocalDef(ann: Annotations, sym: Symbol.VarSym, fparams: Nel[ResolvedAst.FormalParam]) extends Resolution

  case class TypeVar(sym: Symbol.UnkindedTypeVarSym) extends Resolution

  case class Region(sym: Symbol.RegionSym) extends Resolution
}
