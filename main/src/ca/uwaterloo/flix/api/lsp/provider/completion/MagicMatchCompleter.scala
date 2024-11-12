/*
 * Copyright 2024 Chenhao Gao
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.api.lsp.{Position, Range}
import ca.uwaterloo.flix.language.errors.TypeError

object MagicMatchCompleter {

  /**
    * Returns a list of Completions for match, triggered by expr.match.
    *
    * Example:
    * Given an identifier `x` of an enum type `Color` with cases `Red`, `Green`, and `Blue`,
    * typing `x.match` will trigger the completion to expand to:
    *
    * match x {
    *   case Red   => ???
    *   case Green => ???
    *   case Blue  => ???
    * }
    */
  def getCompletions(err: TypeError.FieldNotFound)(implicit root: TypedAst.Root): Iterable[Completion] = {
    for {
      sym <- getEnumSym(err.tpe)
      baseExp <- err.base.text
      cases = root.enums(sym).cases
      if cases.nonEmpty  // We skip empty enums (offer no suggestion)
    } yield {
      val name = s"$baseExp.match"
      val range = sourceLocation2Range(err.loc)
      val casesString = patternMatchBody(cases)
      val snippet = s"match $baseExp {\n$casesString}"
      Completion.MagicMatchCompletion(name, range, snippet, "Expand to a full match expression.")
    }
  }

  /**
    * Returns the length of the case String between "case " and " =>".
    *  Unit:   case Red                 => ???   where the length of "Red" is 3
    *  Tuple:  case Red(_elem1, _elem2) => ???   where the length of "Red(_elem1, _elem2)" is 19
    *  Normal: case Red(_elem)          => ???   where the length of "Red(_elem)" is 10
    */
  private def getCaseLength(cas: TypedAst.Case): Int = {
    cas.tpe.typeConstructor match {
      case Some(TypeConstructor.Unit) => cas.sym.toString.length
      case Some(TypeConstructor.Tuple(arity)) =>
        val numberLength = List.range(1, arity + 1).map(getIntLength).sum
        cas.sym.toString.length + "(_elem,".length * arity + numberLength
      case _ =>
        cas.sym.toString.length + "(_elem)".length
    }
  }

  /**
    * Creates a case string and its corresponding right-hand side string.
    */
  private def createCase(sym: Symbol.CaseSym, cas: TypedAst.Case, z: Int): (String, String, Int) = {
    cas.tpe.typeConstructor match {
      case Some(TypeConstructor.Unit) =>
        (s"$sym", s"$${${z + 1}:???}", z + 1)
      case Some(TypeConstructor.Tuple(arity)) =>
        val elements = List.range(1, arity + 1).map(i => s"$${${i + z}:_elem$i}").mkString(", ")
        (s"$sym($elements)", s"$${${arity + z + 1}:???}", z + arity + 1)
      case _ =>
        (s"$sym($${${z + 1}:_elem})", s"$${${z + 2}:???}", z + 2)
    }
  }

  /**
    * Formats the cases of an enum into a string.
    */
  private def patternMatchBody(cases: Map[Symbol.CaseSym, TypedAst.Case]): String = {
    val maxCaseLength = cases.values.map(getCaseLength).max
    val sb = new StringBuilder
    var z = 1
    for ((sym, cas) <- cases.toList.sortBy(_._1.loc)) {
      val (lhs, rhs, newZ) = createCase(sym, cas, z)
      val paddedLhs = padLhs(lhs, maxCaseLength)
      sb.append(s"    case $paddedLhs => $rhs\n")
      z = newZ
    }
    sb.toString()
  }

  /**
    * Pads the left-hand side (lhs) of a case string to the specified maximum length,
    * accounting for the extra padding required by invisible characters in the case string.
    * For example, "${1:???}" will be displayed as "???", so the extra padding length is 4 plus the length of 1.
    */
  private def padLhs(lhs: String, maxLength: Int): String = {
    val arity = lhs.count(_ == '$')
    val extraPaddingLength = List.range(1, arity + 1).map(4 + getIntLength(_)).sum
    lhs.padTo(maxLength + extraPaddingLength, ' ')
  }

  /**
    * Converts a [[SourceLocation]] to an [[Range]].
    */
  private def sourceLocation2Range(loc: SourceLocation): Range = {
    Range(sourcePosition2Position(loc.sp1), sourcePosition2Position(loc.sp2))
  }

  /**
    * Converts a [[SourcePosition]] to a [[Position]].
    */
  private def sourcePosition2Position(pos: SourcePosition): Position =
    Position(pos.line, pos.col)

  /**
    * Returns the length of the given integer.
    */
  private def getIntLength(i: Int): Int = {
    if (i == 0) 1
    else Math.log10(Math.abs(i)).toInt + 1
  }

  /**
    * Returns the enum symbol of the given type, if it is an enum.
    */
  private def getEnumSym(tpe: Type): Option[Symbol.EnumSym] = tpe.typeConstructor match {
    case Some(TypeConstructor.Enum(sym, _)) => Some(sym)
    case _ => None
  }
}
