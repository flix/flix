/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.shared.{SymbolSet, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Symbol, Type}

import scala.annotation.tailrec

/**
  * Options for formatting types.
  *
  * @param varNames the names that type variables are written by
  * @param qualify  the symbols that are written with their namespace, e.g. `A.B.Name`; every other
  *                 symbol is written by its name alone
  */
case class FormatOptions(varNames: FormatOptions.VarNames, qualify: SymbolSet = SymbolSet.empty)

object FormatOptions {

  /**
    * The format options to use for everything a user reads, e.g. hovers and error messages.
    *
    * Variables are formatted by their names.
    */
  implicit val Default: FormatOptions = FormatOptions(
    varNames = VarNames.BySource
  )

  /**
    * The format options to use for internal
    */
  val Internal: FormatOptions = FormatOptions(
    varNames = VarNames.ById
  )

  /**
    * Returns the default format options with one naming of the type variables of the given types
    * `tpes`, for types that are shown together, e.g. in one error message.
    *
    * See [[VarNames.of]].
    */
  def forTypes(tpes: Type*): FormatOptions = Default.copy(varNames = VarNames.of(tpes))

  /**
    * The names that type variables are written by.
    */
  sealed trait VarNames {

    /**
      * Returns the name of the given type variable `sym`.
      */
    def nameOf(sym: Symbol.KindedTypeVarSym): String = this match {
      case VarNames.ById => VarNames.numbered(sym.kind, sym.id)
      case VarNames.Named(names) => names.getOrElse(sym, sym.text match {
        case VarText.SourceText(s) => s
        case VarText.Absent => VarNames.numbered(sym.kind, sym.id)
      })
    }
  }

  object VarNames {

    /**
      * Indicates that every variable is written by its id, such as `t1234`.
      */
    case object ById extends VarNames

    /**
      * Indicates that a variable is written by its name in `names`. A variable that is not there is
      * written by its source text, such as `res`, and by its id if it has none.
      */
    case class Named(names: Map[Symbol.KindedTypeVarSym, String]) extends VarNames

    /**
      * Indicates that a variable is written by its source text, and by its id if it has none.
      */
    val BySource: VarNames = Named(Map.empty)

    /**
      * Returns one naming of the type variables of the given types `tpes`, for types that are shown
      * together.
      *
      * A variable with a source text is written by it. A variable with none is written by a small
      * number in place of its id, such as `t0`, which is the same in every one of the types. The
      * numbers follow the types in the order given and, within a type, the ids of its variables. A
      * number is skipped if its name is the source text of another variable.
      */
    def of(tpes: Iterable[Type]): VarNames = {
      val syms = tpes.toList.flatMap(_.typeVars.toList.map(_.sym).sortBy(_.id))

      // Two occurrences of a variable can differ in their text: one that has a source text wins.
      val texts = syms.foldLeft(Map.empty[Symbol.KindedTypeVarSym, String]) {
        case (acc, sym) => sym.text match {
          case VarText.SourceText(s) => acc + (sym -> s)
          case VarText.Absent => acc
        }
      }
      val taken = texts.values.toSet

      val (names, _) = syms.distinct.filterNot(texts.contains).foldLeft((texts, 0)) {
        case ((acc, next), sym) =>
          val n = firstFree(sym.kind, next, taken)
          (acc + (sym -> numbered(sym.kind, n)), n + 1)
      }
      Named(names)
    }

    /**
      * Returns the first number from `n` on whose name for a variable of the kind `k` is not in
      * `taken`.
      */
    @tailrec
    private def firstFree(k: Kind, n: Int, taken: Set[String]): Int =
      if (taken.contains(numbered(k, n))) firstFree(k, n + 1, taken) else n

    /**
      * Returns the name of a type variable of the kind `k` with the number `n`: the number with a
      * prefix for the kind, e.g. `t12`, `e3`, `r7`.
      */
    private def numbered(k: Kind, n: Int): String = k match {
      case Kind.Wild => "_" + n
      case Kind.WildCaseSet => "_c" + n
      case Kind.Star => "t" + n
      case Kind.Eff => "e" + n
      case Kind.Bool => "b" + n
      case Kind.RecordRow => "r" + n
      case Kind.SchemaRow => "s" + n
      case Kind.Predicate => "'" + n
      case Kind.Jvm => "j" + n
      case Kind.CaseSet(_) => "c" + n
      case Kind.Arrow(_, _) => "'" + n
      case Kind.Error => "err" + n
    }
  }
}
