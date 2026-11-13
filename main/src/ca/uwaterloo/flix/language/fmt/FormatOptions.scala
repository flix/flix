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
  * @param varFormat the formatting style of variable names
  * @param qualify   the symbols that are written with their namespace, e.g. `A.B.Name`; every other
  *                  symbol is written by its name alone
  */
case class FormatOptions(varFormat: FormatOptions.VarFormat, qualify: SymbolSet = SymbolSet.empty)

object FormatOptions {

  /**
    * The format options to use for everything a user reads, e.g. hovers and error messages.
    *
    * Variables are formatted by their names.
    */
  implicit val Default: FormatOptions = FormatOptions(
    varFormat = VarFormat.NameBased
  )

  /**
    * The format options to use for internal
    */
  val Internal: FormatOptions = FormatOptions(
    varFormat = VarFormat.IdBased
  )

  /**
    * Returns the default format options with the variables of the given types `tpes` numbered, for
    * types that are shown together, e.g. in one error message.
    *
    * See [[VarFormat.Numbered.of]].
    */
  def forTypes(tpes: Type*): FormatOptions = Default.copy(varFormat = VarFormat.Numbered.of(tpes))

  /**
    * An enum for formatting type variables.
    */
  sealed trait VarFormat {

    /**
      * Returns the name of the given type variable `sym`.
      */
    def nameOf(sym: Symbol.KindedTypeVarSym): String = this match {
      case VarFormat.IdBased => VarFormat.idBased(sym)
      case VarFormat.NameBased => VarFormat.nameBased(sym)
      case VarFormat.Numbered(names) => names.getOrElse(sym, VarFormat.nameBased(sym))
    }
  }

  object VarFormat {

    /**
      * Indicates that variables should be formatted by their ID, such as `t1234`.
      */
    case object IdBased extends VarFormat

    /**
      * Indicates that variables should be formatted by their name, such as `res`.
      */
    case object NameBased extends VarFormat

    /**
      * Indicates that variables should be formatted by their name in `names`, such as `t0`, and as
      * under [[NameBased]] if they are not there.
      */
    case class Numbered(names: Map[Symbol.KindedTypeVarSym, String]) extends VarFormat

    object Numbered {

      /**
        * Returns the format that numbers the type variables of the given types `tpes`, for types
        * that are shown together.
        *
        * A variable with a source text is written by it. A variable with none is written by a
        * small number in place of its id, such as `t0`, which is the same in every one of the
        * types. The numbers follow the types in the order given and, within a type, the ids of its
        * variables. A number is skipped if its name is the source text of another variable.
        */
      def of(tpes: Iterable[Type]): Numbered = {
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
            (acc + (sym -> prefixed(sym.kind, n)), n + 1)
        }
        Numbered(names)
      }
    }

    /**
      * Returns the name of the given type variable `sym` by its id, e.g. `t12`, `e3`, `r7`.
      */
    private def idBased(sym: Symbol.KindedTypeVarSym): String = prefixed(sym.kind, sym.id)

    /**
      * Returns the name of the given type variable `sym` by its source text, and by its id if it
      * has none.
      */
    private def nameBased(sym: Symbol.KindedTypeVarSym): String = sym.text match {
      case VarText.Absent => idBased(sym)
      case VarText.SourceText(s) => s
    }

    /**
      * Returns the first number from `n` on whose name for a variable of the kind `k` is not in
      * `taken`.
      */
    @tailrec
    private def firstFree(k: Kind, n: Int, taken: Set[String]): Int =
      if (taken.contains(prefixed(k, n))) firstFree(k, n + 1, taken) else n

    /**
      * Returns the given number `n` with a prefix for the kind `k`, e.g. `t12`, `e3`, `r7`.
      */
    private def prefixed(k: Kind, n: Int): String = k match {
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
