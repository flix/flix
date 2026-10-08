/*
 * Copyright 2021 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.Kind

object FormatKind {

  /**
    * Create a string representation of the kind.
    */
  def formatKind(kind: Kind): String = kind match {
    case Kind.Wild => "???"
    case Kind.WildCaseSet => s"CaseSet[???]"
    case Kind.Star => "Type"
    case Kind.Eff => "Eff"
    case Kind.Bool => "Bool"
    case Kind.RecordRow => "RecordRow"
    case Kind.SchemaRow => "SchemaRow"
    case Kind.Predicate => "Predicate"
    case Kind.Jvm => "Jvm"
    case Kind.CaseSet(sym) => s"CaseSet[${sym.name}]"
    // parenthesize the left because `->` is right-associative
    case Kind.Arrow(k1: Kind.Arrow, k2) => s"(${formatKind(k1)}) -> ${formatKind(k2)}"
    case Kind.Arrow(k1, k2) => s"${formatKind(k1)} -> ${formatKind(k2)}"
    case Kind.Error => "Error"
  }

}
