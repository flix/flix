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
    * Example-1:
    * Given an identifier `x` of an enum type `Color` with cases `Red`, `Green`, and `Blue`,
    * typing `x.match` will trigger the completion to expand to:
    *
    * match x {
    *   case Red   => ???
    *   case Green => ???
    *   case Blue  => ???
    * }
    *
    * Example-2:
    * Given an identifier `x` of a tuple type `(Color, Shape)` with cases `Red` and `Green` for Color` and `Circle`, `Square` for `Shape`,
    * typing `x.match` will trigger the completion to expand to:
    *
    * match x {
    *  case (Red, Circle)   => ???
    *  case (Red, Square)   => ???
    *  case (Green, Circle) => ???
    *  case (Green, Square) => ???
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
    * Formats the cases of an enum into the pattern match body
    */
  private def mkEnumMatchBody(cases: Map[Symbol.CaseSym, TypedAst.Case]): String = {
    val maxCaseLength = cases.values.map(getCaseLength).max
    val sb = new StringBuilder
    var index = 1
    for ((sym, cas) <- cases.toList.sortBy(_._1.loc)) {
      val (lhs, newIndex) = createCase(sym, cas, index)
      val paddedLhs = padLhs(lhs, maxCaseLength, index, newIndex)
      sb.append(s"    case $paddedLhs => $${$newIndex:???}\n")
      index = newIndex + 1
    }
    sb.toString()
  }

  /**
    * Converts the given type into a Member.
    * If the type is an enum, the Member consists of EnumMemberItem instances.
    * Otherwise, the Member is a single-element list containing an OtherMemberItem with a string indicating the index of the member.
    */
  private def type2member(tpe: Type, idx: Int)(implicit root: TypedAst.Root): Member =
    getEnumSym(tpe) match {
      case Some(sym) => root.enums(sym).cases.toList.map{case (sym, cas) => EnumMemberItem(sym, cas)}
      case None => OtherMemberItem(s"_member$idx") :: Nil
    }

  /**
    * Returns the cartesian product of the given list of Members.
    * The length of the returned list is the product of the lengths of each Member.
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
    * Formats the cases of a tuple into the pattern match body.
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
    * accounting for the extra padding length required by invisible placeholders in the case string.
    * For example, "${13:???}" will be displayed as "???", so the extra padding length is 4 plus the length of "13".
    * We use the input oldIndex and newIndex to track all the placeholders in the case string.
    */
  private def padLhs(lhs: String, maxLength: Int, oldIndex: Int, newIndex: Int): String = {
    val extraPaddingLength = List.range(oldIndex, newIndex).map(4 + getIntLength(_)).sum
    lhs.padTo(maxLength + extraPaddingLength, ' ')
  }

  /**
    * Returns the length of the case String between "case " and " =>".
    *  Unit:   case Color.Red                 => ???   where the length of "Color.Red" is 9
    *  Tuple:  case Color.Red(_elem1, _elem2) => ???   where the length of "Color.Red(_elem1, _elem2)" is 25
    *  Normal: case Color.Red(_elem)          => ???   where the length of "Color.Red(_elem)" is 16
    */
  private def getCaseLength(cas: TypedAst.Case): Int = {
    cas.tpes match {
      case Nil => cas.sym.toString.length
      case List(_) =>
        cas.sym.toString.length + "(_elem)".length
      case other =>
        val arity = other.length
        val numberLength = List.range(1, arity + 1).map(getIntLength).sum
        cas.sym.toString.length + "(_elem,".length * arity + numberLength
    }
  }

  /**
    * Creates a case string and its corresponding right-hand side string.
    */
  private def createCase(sym: Symbol.CaseSym, cas: TypedAst.Case, index: Int): (String, Int) = {
    cas.tpes match {
      case Nil =>
        (s"$sym", index)
      case List(_) =>
        (s"$sym($${${index}:_elem})", index + 1)
      case other =>
        val arity = other.length
        val elements = List.range(0, arity).map(i => s"$${${i + index}:_elem$i}").mkString(", ")
        (s"$sym($elements)",  index + arity)
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
