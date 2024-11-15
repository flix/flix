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
      baseExp <- err.base.text
      patternMatchBody <- mkPatternMatchBody(err.tpe)
    } yield {
      val name = s"$baseExp.match"
      val range = sourceLocation2Range(err.loc)
      val snippet = s"match $baseExp {\n$patternMatchBody}"
      Completion.MagicMatchCompletion(name, range, snippet, "match expr { ... }")
    }
  }

  /**
    * Returns the pattern match body for the given type.
    * Currently, only enums and tuples are supported.
    */
  private def mkPatternMatchBody(tpe: Type)(implicit root: TypedAst.Root): Option[String] = {
    tpe.typeConstructor match {
      case Some(TypeConstructor.Enum(sym, _)) =>
        val cases = root.enums(sym).cases
        if (cases.nonEmpty) Some(mkEnumMatchBody(cases)) else None
      case Some(TypeConstructor.Tuple(_)) =>
        val memberList = tpe.typeArguments.zipWithIndex.map { case (tpe, idx) => type2member(tpe, idx) }
        val memberCombinations = cartesianProduct(memberList)
        Some(mkTupleMatchBody(memberCombinations))
      case _ =>
        None
    }
  }

  /**
    * Formats the cases of an enum into a string.
    */
  private def mkEnumMatchBody(cases: Map[Symbol.CaseSym, TypedAst.Case]): String = {
    val maxCaseLength = cases.values.map(getCaseLength).max
    val sb = new StringBuilder
    var index = 1
    for ((sym, cas) <- cases.toList.sortBy(_._1.loc)) {
      val oldIndex = index
      val (lhs, newZ) = createCase(sym, cas, index)
      val paddedLhs = padLhs(lhs, maxCaseLength, oldIndex, newZ)
      sb.append(s"    case $paddedLhs => $${$index:???}\n")
      index = newZ + 1
    }
    sb.toString()
  }

  /**
    * Convert the given type into a list of Member.
    * If the given type is an enum, return a list of EnumCase.
    * Otherwise, return a list of SingleCase, which contains only a string indicating the idx of the member.
    */
  private def type2member(tpe: Type, idx: Int)(implicit root: TypedAst.Root): Member =
    getEnumSym(tpe) match {
      case Some(sym) => root.enums(sym).cases.toList.map{case (sym, cas) => EnumMemberItem(sym, cas)}
      case None => OtherMemberItem(s"_member$idx") :: Nil
    }

  /**
    * Returns the cartesian product of the given list of lists.
    * The length of the returned list is the product of the lengths of the input lists.
    *
    * Example:
    *  Given [ [Red, Green], [Circle, Square] ]
    *  The cartesian product is [ [Red, Circle], [Red, Square], [Green, Circle], [Green, Square] ]
    */
  private def cartesianProduct(casesPerType: List[Member]): List[Member] = {
    casesPerType.foldLeft(List(List.empty[MemberItem])) { (acc, list) =>
      for {
        x <- acc
        y <- list
      } yield x :+ y
    }
  }

  /**
    * Formats the cases of a tuple into a string.
    */
  private def mkTupleMatchBody(memberCombinations: List[Member]): String = {
    val sb = new StringBuilder
    val maxCombinationLength = memberCombinations.map(getMemberLength).max
    var index = 1

    memberCombinations.foreach { cases =>
      sb.append("    case ")
      val oldIndex = index
      val lhs = cases.map {
        case EnumMemberItem(sym, cas) =>
          val (lhs, newZ) = createCase(sym, cas, index)
          index = newZ
          lhs
        case OtherMemberItem(s) =>
          val placeholder = s"$${$index:$s}"
          index += 1
          placeholder
      }.mkString(", ")
      val paddedLhs = padLhs(s"($lhs)", maxCombinationLength, oldIndex, index)
      sb.append(s"$paddedLhs => $${$index:???}\n")
      index += 1
    }
    sb.toString()
  }

  /**
    * Returns the length of a single member string
    * The member string has three forms:
    *  (String,
    *   String,
    *   String)
    * Thus an extra length of 2 is added to the length of the member string.
    */
  private def getMemberLength(cases: Member): Int = {
    cases.map {
      case EnumMemberItem(_, cas) => getCaseLength(cas) + 2
      case OtherMemberItem(s) => s.length + 2
    }.sum
  }

  /**
    * Pads the middle of a tuple case string to the specified maximum length,
    * accounting for the extra padding required by invisible characters in the case string.
    * For example, "${1:???}" will be displayed as "???", so the extra padding length is 4 plus the length of 1.
    */
  private def padLhs(lhs: String, maxLength: Int, z1: Int, z: Int): String = {
    val extraPaddingLength = List.range(z1, z).map(4 + getIntLength(_)).sum
    lhs.padTo(maxLength + extraPaddingLength, ' ')
  }

  /**
    * Returns the length of the case String between "case " and " =>".
    *  Unit:   case Color.Red                 => ???   where the length of "Color.Red" is 9
    *  Tuple:  case Color.Red(_elem1, _elem2) => ???   where the length of "Color.Red(_elem1, _elem2)" is 25
    *  Normal: case Color.Red(_elem)          => ???   where the length of "Color.Red(_elem)" is 16
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
  private def createCase(sym: Symbol.CaseSym, cas: TypedAst.Case, z: Int): (String, Int) = {
    cas.tpe.typeConstructor match {
      case Some(TypeConstructor.Unit) =>
        (s"$sym", z)
      case Some(TypeConstructor.Tuple(arity)) =>
        val elements = List.range(0, arity).map(i => s"$${${i + z}:_elem$i}").mkString(", ")
        (s"$sym($elements)",  z + arity)
      case _ =>
        (s"$sym($${${z}:_elem})", z + 1)
    }
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

  /**
    * Represents a member in a tuple.
    */
  type Member = List[MemberItem]

  /**
    * Represents a member item inside a member.
    * For an enum type, it is a case of the enum.
    * For a non-enum type, it is a placeholder string.
    */
  sealed trait MemberItem

  /**
    * Represents a case of an enum type.
    */
  private case class EnumMemberItem(sym: Symbol.CaseSym, cas: TypedAst.Case) extends MemberItem

  /**
    * Represents any non-enum type in a tuple
    * We will use a placeholder string "_member0" to represent the member.
    */
  private case class OtherMemberItem(caseName: String) extends MemberItem
}
