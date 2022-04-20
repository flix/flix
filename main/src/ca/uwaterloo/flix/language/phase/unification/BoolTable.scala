/*
 * Copyright 2022 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.fmt.{Audience, FormatType}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable.ListBuffer

object BoolTable {

  /**
    * A Boolean variable is represented by a unique number.
    */
  private type Variable = Int

  /**
    * A table that maps Boolean semantic functions to their minimal formulas.
    *
    * The table is pre-computed and initialized when this class is loaded.
    */
  private lazy val cache: Map[Int, Formula] = buildTable()

  /**
    * A common super-type for Boolean formulas.
    */
  private sealed trait Formula {

    /**
      * Returns the size of `this` formulas.
      *
      * The size is the number of conjunctions and disjunctions.
      */
    final def size: Int = this match {
      case Formula.True => 0
      case Formula.False => 0
      case Formula.Var(_) => 0
      case Formula.Neg(t) => t.size
      case Formula.Conj(t1, t2) => t1.size + t2.size + 1
      case Formula.Disj(t1, t2) => t1.size + t2.size + 1
    }

    /**
      * Returns a human-readable fully parenthesized string representation of `this` term.
      */
    override def toString: String = this match {
      case Formula.True => "true"
      case Formula.False => "false"
      case Formula.Var(x) => s"x$x"
      case Formula.Neg(t) => s"not $t"
      case Formula.Conj(t1, t2) => s"($t1 and $t2)"
      case Formula.Disj(t1, t2) => s"($t1 or $t2)"
    }

  }

  private object Formula {
    /**
      * Represents the constant True.
      */
    case object True extends Formula

    /**
      * Represents the constant False.
      */
    case object False extends Formula

    /**
      * Represents a vairable.
      */
    case class Var(x: Variable) extends Formula

    /**
      * Represents the negation of the formula `t`.
      */
    case class Neg(t: Formula) extends Formula

    /**
      * Represents the conjunction (logical and) of `t1` and `t2`.
      */
    case class Conj(t1: Formula, t2: Formula) extends Formula

    /**
      * Represents the disjunction (logical or) of `t1` and `t2`.
      */
    case class Disj(t1: Formula, t2: Formula) extends Formula
  }

  /**
    * Attempts to minimize the given Boolean formulas `tpe`.
    *
    * Returns the same formula or a smaller formula that is equivalent.
    *
    * @param tpe the formulas to minimize. Must have kind `Bool`.
    */
  def minimize(tpe: Type): Type = {
    //
    // Check that the given type argument is a Boolean formula.
    //
    if (tpe.kind != Kind.Bool) {
      throw InternalCompilerException(s"Unexpected non-Bool kind: '${tpe.kind}'.")
    }

    val tvars = tpe.typeVars.map(_.sym).toList
    if (tpe.size < 8 || tvars.size > 5) {
      return tpe
    }

    val typeVarMap = tvars.zipWithIndex.toMap
    val reverseTypeVarMap = typeVarMap.map(_.swap).toMap

    //println(s"type vars: ${tvars.size}")

    val t = fromType(tpe, typeVarMap)

    val freeVars = tvars.indices.toList
    val semantic = semanticFunction(0, t, freeVars, Map.empty)

    //val fmtFormula = FormatType.formatWellKindedType(tpe)(Audience.External).take(80)
    //val fmtBinary = semantic.toBinaryString
    //println(s"$fmtFormula:  $fmtBinary")

    cache.get(semantic) match {
      case None => toType(t, reverseTypeVarMap)
      case Some(result) =>
        val currentSize = t.size
        val minimalSize = result.size

        val minimal = toType(result, reverseTypeVarMap)
        if (minimalSize < currentSize) {
          implicit val audience: Audience = Audience.Internal
          println(s"Replace: ${FormatType.formatWellKindedType(tpe)}")
          println(s"     By: ${FormatType.formatWellKindedType(minimal)}")
          println(s" Reduct: $currentSize -> $minimalSize")
          println()
        }

        minimal
    }
  }


  private def semanticFunction(position: Int, t0: Formula, fvs: List[Variable], binding: Map[Variable, Boolean]): Int = fvs match {
    case Nil => if (eval(t0, binding)) 1 << position else 0
    case x :: xs =>
      val l = semanticFunction(position, t0, xs, binding + (x -> true))
      val r = semanticFunction(position + (1 << (fvs.length - 1)), t0, xs, binding + (x -> false))
      l | r
  }

  private def eval(f: Formula, env: Map[Variable, Boolean]): Boolean = f match {
    case Formula.True => true
    case Formula.False => false
    case Formula.Var(sym) => env(sym)
    case Formula.Neg(t) => !eval(t, env)
    case Formula.Conj(t1, t2) => eval(t1, env) && eval(t2, env)
    case Formula.Disj(t1, t2) => eval(t1, env) || eval(t2, env)
  }

  private def fromType(tpe0: Type, m: Map[Symbol.KindedTypeVarSym, Variable]): Formula = tpe0 match {
    case Type.KindedVar(sym, _) => Formula.Var(m(sym))
    case Type.True => Formula.True
    case Type.False => Formula.False
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), t, _) => Formula.Neg(fromType(t, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), t1, _), t2, _) => Formula.Conj(fromType(t1, m), fromType(t2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), t1, _), t2, _) => Formula.Disj(fromType(t1, m), fromType(t2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe0'.")
  }

  def toType(t0: Formula, m: Map[Variable, Symbol.KindedTypeVarSym]): Type = t0 match {
    case Formula.True => Type.True
    case Formula.False => Type.False
    case Formula.Var(x) => Type.KindedVar(m(x), SourceLocation.Unknown)
    case Formula.Neg(t) => Type.mkNot(toType(t, m), SourceLocation.Unknown)
    case Formula.Conj(t1, t2) => Type.mkAnd(toType(t1, m), toType(t2, m), SourceLocation.Unknown)
    case Formula.Disj(t1, t2) => Type.mkOr(toType(t1, m), toType(t2, m), SourceLocation.Unknown)
  }

  private def buildTable(): Map[Int, Formula] = {
    val table = ExpressionParser.parse(table3)
    parseTable(table)
    // prettyPrintLookupTable()
  }

  def prettyPrintLookupTable(): Unit = {
    for ((key, term) <- cache) {
      println(s"${key.toBinaryString.padTo(8, '0')}: $term")
    }
    println(s"size = ${cache.size}")
  }

  private def parseTable(l: SList): Map[Int, Formula] = l match {
    case SList(elms) => elms.tail.map(parseKeyValue).toMap
  }

  private def parseKeyValue(elm: Element): (Int, Formula) = elm match {
    case SList(List(Atom(key), formula)) => parseKey(key) -> parseFormula(formula)
    case _ => throw InternalCompilerException(s"Parse Error. Unexpected element: '$elm'.")
  }

  def parseKey(key: String): Int = {
    var result = 0
    for ((c, position) <- key.zipWithIndex) {
      if (c == 'T') {
        result = result | (1 << (position - 1))
      }
    }
    result
  }

  private def parseFormula(elm: Element): Formula = elm match {
    case Atom("T") => Formula.True
    case Atom("F") => Formula.False
    case Atom("x0") => Formula.Var(0)
    case Atom("x1") => Formula.Var(1)
    case Atom("x2") => Formula.Var(2)
    case SList(List(Atom("not"), x)) => Formula.Neg(parseFormula(x))
    case SList(List(Atom("and"), x, y)) => Formula.Conj(parseFormula(x), parseFormula(y))
    case SList(List(Atom("or"), x, y)) => Formula.Disj(parseFormula(x), parseFormula(y))
    case _ => throw InternalCompilerException(s"Parse Error. Unexpected element: '$elm'.")
  }

  // https://github.com/ZenBowman/sexpr
  // The MIT License (MIT)
  //
  //Copyright (c) 2013 ZenBowman
  //

  class InvalidSExpressionException extends Exception

  sealed trait Element

  case class Atom(symbol: String) extends Element

  case class SList(values: List[Element]) extends Element

  object ExpressionParser {
    private var remainingTokens: List[String] = List()

    def tokenize(expression: String): List[String] = {
      expression.replace("(", " ( ").replace(")", " ) ").trim().split("\\s+").toList
    }

    def parse(expression: String): SList = {
      remainingTokens = tokenize(expression)
      parseTokens()
    }

    def parseTokens(): SList = {
      val elements = new ListBuffer[Element]

      while (!remainingTokens.isEmpty) {
        val first = remainingTokens.head
        remainingTokens = remainingTokens.tail
        if (first == "(") {
          val element = parseTokens()
          elements.append(element)
        }
        else if (first == ")") {
          return SList(elements.toList)
        } else {
          elements.append(new Atom(first))
        }
      }

      try {
        elements.head.asInstanceOf[SList]
      } catch {
        case _: Exception => throw new InvalidSExpressionException
      }
    }
  }

  private val table3: String =
    """(table
      |("FFFFFFFF" F)
      |("TFFFFFFF" (and x0 (and x1 x2)))
      |("FTFFFFFF" (and x0 (and x1 (not x2))))
      |("TTFFFFFF" (and x0 x1))
      |("FFTFFFFF" (and x0 (and (not x1) x2)))
      |("TFTFFFFF" (and x0 x2))
      |("FTTFFFFF" (and x0 (and (or (not x1) (not x2)) (or x1 x2))))
      |("TTTFFFFF" (and x0 (or x1 x2)))
      |("FFFTFFFF" (and x0 (and (not x1) (not x2))))
      |("TFFTFFFF" (and x0 (or (and (not x1) (not x2)) (and x1 x2))))
      |("FTFTFFFF" (and x0 (not x2)))
      |("TTFTFFFF" (and x0 (or x1 (not x2))))
      |("FFTTFFFF" (and x0 (not x1)))
      |("TFTTFFFF" (and x0 (or (not x1) x2)))
      |("FTTTFFFF" (and x0 (or (not x1) (not x2))))
      |("TTTTFFFF" x0)
      |("FFFFTFFF" (and (not x0) (and x1 x2)))
      |("TFFFTFFF" (and x1 x2))
      |("FTFFTFFF" (and x1 (and (or (not x0) (not x2)) (or x0 x2))))
      |("TTFFTFFF" (and x1 (or x0 x2)))
      |("FFTFTFFF" (and x2 (and (or (not x0) (not x1)) (or x0 x1))))
      |("TFTFTFFF" (and x2 (or x0 x1)))
      |("FTTFTFFF" (and (or x0 x1) (and (or (not x0) (or (not x1) (not x2))) (or x2 (and x0 x1)))))
      |("TTTFTFFF" (or (and x0 x1) (and x2 (or x0 x1))))
      |("FFFTTFFF" (and (or (not x0) (not x1)) (or (and x0 (not x2)) (and x1 x2))))
      |("TFFTTFFF" (and (or (not x1) x2) (or x1 (and x0 (not x2)))))
      |("FTFTTFFF" (and (or (not x0) (not x2)) (or x0 (and x1 x2))))
      |("TTFTTFFF" (or (and x0 (not x2)) (and x1 x2)))
      |("FFTTTFFF" (and (or (not x0) (not x1)) (or x0 (and x1 x2))))
      |("TFTTTFFF" (or (and x0 (not x1)) (and x1 x2)))
      |("FTTTTFFF" (and (or (not x0) (or (not x1) (not x2))) (or x0 (and x1 x2))))
      |("TTTTTFFF" (or x0 (and x1 x2)))
      |("FFFFFTFF" (and (not x0) (and x1 (not x2))))
      |("TFFFFTFF" (and x1 (or (and (not x0) (not x2)) (and x0 x2))))
      |("FTFFFTFF" (and x1 (not x2)))
      |("TTFFFTFF" (and x1 (or x0 (not x2))))
      |("FFTFFTFF" (and (or (not x0) (not x1)) (and (or x0 (not x2)) (or x1 x2))))
      |("TFTFFTFF" (and (or (not x0) x2) (or x0 (and x1 (not x2)))))
      |("FTTFFTFF" (and (or (not x1) (not x2)) (or x1 (and x0 x2))))
      |("TTTFFTFF" (and (or x0 (not x2)) (or x1 x2)))
      |("FFFTFTFF" (and (not x2) (and (or (not x0) (not x1)) (or x0 x1))))
      |("TFFTFTFF" (and (or x0 x1) (and (or (not x0) (or (not x1) x2)) (or (not x2) (and x0 x1)))))
      |("FTFTFTFF" (and (not x2) (or x0 x1)))
      |("TTFTFTFF" (or (and x0 x1) (and (not x2) (or x0 x1))))
      |("FFTTFTFF" (and (or (not x0) (not x1)) (or x0 (and x1 (not x2)))))
      |("TFTTFTFF" (and (or (not x0) (or (not x1) x2)) (or x0 (and x1 (not x2)))))
      |("FTTTFTFF" (or (and x0 (not x1)) (and x1 (not x2))))
      |("TTTTFTFF" (or x0 (and x1 (not x2))))
      |("FFFFTTFF" (and (not x0) x1))
      |("TFFFTTFF" (and x1 (or (not x0) x2)))
      |("FTFFTTFF" (and x1 (or (not x0) (not x2))))
      |("TTFFTTFF" x1)
      |("FFTFTTFF" (and (or (not x0) (not x1)) (or x1 (and x0 x2))))
      |("TFTFTTFF" (or (and (not x0) x1) (and x0 x2)))
      |("FTTFTTFF" (and (or (not x0) (or (not x1) (not x2))) (or x1 (and x0 x2))))
      |("TTTFTTFF" (or x1 (and x0 x2)))
      |("FFFTTTFF" (and (or (not x0) (not x1)) (or x1 (and x0 (not x2)))))
      |("TFFTTTFF" (and (or (not x0) (or (not x1) x2)) (or x1 (and x0 (not x2)))))
      |("FTFTTTFF" (or (and (not x0) x1) (and x0 (not x2))))
      |("TTFTTTFF" (or x1 (and x0 (not x2))))
      |("FFTTTTFF" (and (or (not x0) (not x1)) (or x0 x1)))
      |("TFTTTTFF" (or (and (not x0) x1) (and x0 (or (not x1) x2))))
      |("FTTTTTFF" (or (and (not x0) x1) (and x0 (or (not x1) (not x2)))))
      |("TTTTTTFF" (or x0 x1))
      |("FFFFFFTF" (and (not x0) (and (not x1) x2)))
      |("TFFFFFTF" (and x2 (or (and (not x0) (not x1)) (and x0 x1))))
      |("FTFFFFTF" (and (or (not x0) x1) (or (and x0 (not x2)) (and (not x1) x2))))
      |("TTFFFFTF" (and (or (not x0) x1) (or x0 (and (not x1) x2))))
      |("FFTFFFTF" (and (not x1) x2))
      |("TFTFFFTF" (and x2 (or x0 (not x1))))
      |("FTTFFFTF" (and (or (not x1) (not x2)) (or x2 (and x0 x1))))
      |("TTTFFFTF" (and (or x0 (not x1)) (or x1 x2)))
      |("FFFTFFTF" (and (not x1) (and (or (not x0) (not x2)) (or x0 x2))))
      |("TFFTFFTF" (and (or x0 (not x1)) (and (or (not x0) (or x1 (not x2))) (or x2 (and x0 (not x1))))))
      |("FTFTFFTF" (and (or (not x0) (not x2)) (or x0 (and (not x1) x2))))
      |("TTFTFFTF" (or (and (not x0) (and (not x1) x2)) (and x0 (or x1 (not x2)))))
      |("FFTTFFTF" (and (not x1) (or x0 x2)))
      |("TFTTFFTF" (or (and x0 (not x1)) (and x2 (or x0 (not x1)))))
      |("FTTTFFTF" (or (and x0 (not x2)) (and (not x1) x2)))
      |("TTTTFFTF" (or x0 (and (not x1) x2)))
      |("FFFFTFTF" (and (not x0) x2))
      |("TFFFTFTF" (and x2 (or (not x0) x1)))
      |("FTFFTFTF" (and (or (not x0) (not x2)) (or x2 (and x0 x1))))
      |("TTFFTFTF" (and (or (not x0) x1) (or x0 x2)))
      |("FFTFTFTF" (and x2 (or (not x0) (not x1))))
      |("TFTFTFTF" x2)
      |("FTTFTFTF" (and (or (not x0) (or (not x1) (not x2))) (or x2 (and x0 x1))))
      |("TTTFTFTF" (or x2 (and x0 x1)))
      |("FFFTTFTF" (and (or (not x0) (not x2)) (or x2 (and x0 (not x1)))))
      |("TFFTTFTF" (and (or (not x0) (or x1 (not x2))) (or x2 (and x0 (not x1)))))
      |("FTFTTFTF" (and (or (not x0) (not x2)) (or x0 x2)))
      |("TTFTTFTF" (or (and (not x0) x2) (and x0 (or x1 (not x2)))))
      |("FFTTTFTF" (and (or (not x0) (not x1)) (or x0 x2)))
      |("TFTTTFTF" (or x2 (and x0 (not x1))))
      |("FTTTTFTF" (or (and (not x0) x2) (and x0 (or (not x1) (not x2)))))
      |("TTTTTFTF" (or x0 x2))
      |("FFFFFTTF" (and (not x0) (and (or (not x1) (not x2)) (or x1 x2))))
      |("TFFFFTTF" (and (or (not x0) x1) (or (and (not x0) (and x1 (not x2))) (and x2 (or x0 (not x1))))))
      |("FTFFFTTF" (and (or (not x1) (not x2)) (or x1 (and (not x0) x2))))
      |("TTFFFTTF" (or (and (not x0) (and (not x1) x2)) (and x1 (or x0 (not x2)))))
      |("FFTFFTTF" (and (or (not x1) (not x2)) (or x2 (and (not x0) x1))))
      |("TFTFFTTF" (or (and (not x0) (and x1 (not x2))) (and x2 (or x0 (not x1)))))
      |("FTTFFTTF" (and (or (not x1) (not x2)) (or x1 x2)))
      |("TTTFFTTF" (or (and (not x1) x2) (and x1 (or x0 (not x2)))))
      |("FFFTFTTF" (and (or (not x0) (not x1)) (or (and (not x0) (and (not x1) x2)) (and (not x2) (or x0 x1)))))
      |("TFFTFTTF" (and (or (not x0) (or (and (not x1) (not x2)) (and x1 x2))) (or x0 (and (or (not x1) (not x2)) (or x1 x2)))))
      |("FTFTFTTF" (or (and (not x0) (and (not x1) x2)) (and (not x2) (or x0 x1))))
      |("TTFTFTTF" (or (and x0 x1) (or (and (not x0) (and (not x1) x2)) (and (not x2) (or x0 x1)))))
      |("FFTTFTTF" (or (and (not x0) (and x1 (not x2))) (and (not x1) (or x0 x2))))
      |("TFTTFTTF" (or (and x0 (not x1)) (or (and (not x0) (and x1 (not x2))) (and x2 (or x0 (not x1))))))
      |("FTTTFTTF" (and (or (not x1) (not x2)) (or x0 (or x1 x2))))
      |("TTTTFTTF" (or x0 (and (or (not x1) (not x2)) (or x1 x2))))
      |("FFFFTTTF" (and (not x0) (or x1 x2)))
      |("TFFFTTTF" (or (and (not x0) x1) (and x2 (or (not x0) x1))))
      |("FTFFTTTF" (and (or (not x0) (not x2)) (or x1 x2)))
      |("TTFFTTTF" (or x1 (and (not x0) x2)))
      |("FFTFTTTF" (and (or (not x0) (not x1)) (or x1 x2)))
      |("TFTFTTTF" (or x2 (and (not x0) x1)))
      |("FTTFTTTF" (or (and (not x1) x2) (and x1 (or (not x0) (not x2)))))
      |("TTTFTTTF" (or x1 x2))
      |("FFFTTTTF" (and (or (not x0) (and (not x1) (not x2))) (or x0 (or x1 x2))))
      |("TFFTTTTF" (or (and (not x0) x1) (and (or (not x0) (or x1 (not x2))) (or x2 (and x0 (not x1))))))
      |("FTFTTTTF" (and (or (not x0) (not x2)) (or x0 (or x1 x2))))
      |("TTFTTTTF" (or x1 (and (or (not x0) (not x2)) (or x0 x2))))
      |("FFTTTTTF" (and (or (not x0) (not x1)) (or x0 (or x1 x2))))
      |("TFTTTTTF" (or x2 (and (or (not x0) (not x1)) (or x0 x1))))
      |("FTTTTTTF" (or (and (not x0) x1) (or (and x0 (not x2)) (and (not x1) x2))))
      |("TTTTTTTF" (or x0 (or x1 x2)))
      |("FFFFFFFT" (and (not x0) (and (not x1) (not x2))))
      |("TFFFFFFT" (and (or (not x0) x1) (and (or x0 (not x2)) (or (not x1) x2))))
      |("FTFFFFFT" (and (not x2) (or (and (not x0) (not x1)) (and x0 x1))))
      |("TTFFFFFT" (and (or (not x0) x1) (or x0 (and (not x1) (not x2)))))
      |("FFTFFFFT" (and (not x1) (or (and (not x0) (not x2)) (and x0 x2))))
      |("TFTFFFFT" (and (or (not x0) x2) (or x0 (and (not x1) (not x2)))))
      |("FTTFFFFT" (and (or x0 (not x1)) (and (or (not x0) (or x1 x2)) (or (not x2) (and x0 (not x1))))))
      |("TTTFFFFT" (or (and (not x0) (and (not x1) (not x2))) (and x0 (or x1 x2))))
      |("FFFTFFFT" (and (not x1) (not x2)))
      |("TFFTFFFT" (or (and (not x1) (not x2)) (and x0 (and x1 x2))))
      |("FTFTFFFT" (and (not x2) (or x0 (not x1))))
      |("TTFTFFFT" (and (or x0 (not x1)) (or x1 (not x2))))
      |("FFTTFFFT" (and (not x1) (or x0 (not x2))))
      |("TFTTFFFT" (and (or x0 (not x2)) (or (not x1) x2)))
      |("FTTTFFFT" (or (and x0 (not x1)) (and (not x2) (or x0 (not x1)))))
      |("TTTTFFFT" (or x0 (and (not x1) (not x2))))
      |("FFFFTFFT" (and (not x0) (or (and (not x1) (not x2)) (and x1 x2))))
      |("TFFFTFFT" (and (or (not x1) x2) (or x1 (and (not x0) (not x2)))))
      |("FTFFTFFT" (and (or (not x0) x1) (or (and (not x0) (and x1 x2)) (and (not x2) (or x0 (not x1))))))
      |("TTFFTFFT" (or (and (not x0) (and (not x1) (not x2))) (and x1 (or x0 x2))))
      |("FFTFTFFT" (and (or (not x0) (not x1)) (or (and (not x0) (and (not x1) (not x2))) (and x2 (or x0 x1)))))
      |("TFTFTFFT" (or (and (not x0) (and (not x1) (not x2))) (and x2 (or x0 x1))))
      |("FTTFTFFT" (or (and (not x0) (or (and (not x1) (not x2)) (and x1 x2))) (and x0 (and (or (not x1) (not x2)) (or x1 x2)))))
      |("TTTFTFFT" (or (and x0 x1) (or (and (not x0) (and (not x1) (not x2))) (and x2 (or x0 x1)))))
      |("FFFTTFFT" (or (and (not x1) (not x2)) (and (not x0) (and x1 x2))))
      |("TFFTTFFT" (or (and (not x1) (not x2)) (and x1 x2)))
      |("FTFTTFFT" (or (and (not x0) (and x1 x2)) (and (not x2) (or x0 (not x1)))))
      |("TTFTTFFT" (or (and (not x1) (not x2)) (and x1 (or x0 x2))))
      |("FFTTTFFT" (or (and (not x0) (and x1 x2)) (and (not x1) (or x0 (not x2)))))
      |("TFTTTFFT" (or (and (not x1) (not x2)) (and x2 (or x0 x1))))
      |("FTTTTFFT" (or (and x0 (not x1)) (or (and (not x0) (and x1 x2)) (and (not x2) (or x0 (not x1))))))
      |("TTTTTFFT" (or x0 (or (and (not x1) (not x2)) (and x1 x2))))
      |("FFFFFTFT" (and (not x0) (not x2)))
      |("TFFFFTFT" (or (and (not x0) (not x2)) (and x0 (and x1 x2))))
      |("FTFFFTFT" (and (not x2) (or (not x0) x1)))
      |("TTFFFTFT" (and (or (not x0) x1) (or x0 (not x2))))
      |("FFTFFTFT" (or (and (not x0) (not x2)) (and x0 (and (not x1) x2))))
      |("TFTFFTFT" (or (and (not x0) (not x2)) (and x0 x2)))
      |("FTTFFTFT" (and (or (not x0) (or x1 x2)) (or (not x2) (and x0 (not x1)))))
      |("TTTFFTFT" (or (and (not x0) (not x2)) (and x0 (or x1 x2))))
      |("FFFTFTFT" (and (not x2) (or (not x0) (not x1))))
      |("TFFTFTFT" (and (or (not x0) (or (not x1) x2)) (or (not x2) (and x0 x1))))
      |("FTFTFTFT" (not x2))
      |("TTFTFTFT" (or (not x2) (and x0 x1)))
      |("FFTTFTFT" (and (or (not x0) (not x1)) (or x0 (not x2))))
      |("TFTTFTFT" (or (and (not x0) (not x2)) (and x0 (or (not x1) x2))))
      |("FTTTFTFT" (or (not x2) (and x0 (not x1))))
      |("TTTTFTFT" (or x0 (not x2)))
      |("FFFFTTFT" (and (not x0) (or x1 (not x2))))
      |("TFFFTTFT" (or (and (not x0) (not x2)) (and x1 x2)))
      |("FTFFTTFT" (or (and (not x0) x1) (and (not x2) (or (not x0) x1))))
      |("TTFFTTFT" (or x1 (and (not x0) (not x2))))
      |("FFTFTTFT" (and (or (not x0) (and (not x1) x2)) (or x0 (or x1 (not x2)))))
      |("TFTFTTFT" (or (and (not x0) (not x2)) (and x2 (or x0 x1))))
      |("FTTFTTFT" (or (and (not x0) x1) (and (or (not x0) (or x1 x2)) (or (not x2) (and x0 (not x1))))))
      |("TTTFTTFT" (or x1 (or (and (not x0) (not x2)) (and x0 x2))))
      |("FFFTTTFT" (and (or (not x0) (not x1)) (or x1 (not x2))))
      |("TFFTTTFT" (or (and (not x1) (not x2)) (and x1 (or (not x0) x2))))
      |("FTFTTTFT" (or (not x2) (and (not x0) x1)))
      |("TTFTTTFT" (or x1 (not x2)))
      |("FFTTTTFT" (and (or (not x0) (not x1)) (or x0 (or x1 (not x2)))))
      |("TFTTTTFT" (or (and (not x0) x1) (and (or x0 (not x2)) (or (not x1) x2))))
      |("FTTTTTFT" (or (not x2) (and (or (not x0) (not x1)) (or x0 x1))))
      |("TTTTTTFT" (or x0 (or x1 (not x2))))
      |("FFFFFFTT" (and (not x0) (not x1)))
      |("TFFFFFTT" (or (and (not x0) (not x1)) (and x0 (and x1 x2))))
      |("FTFFFFTT" (or (and (not x0) (not x1)) (and x0 (and x1 (not x2)))))
      |("TTFFFFTT" (or (and (not x0) (not x1)) (and x0 x1)))
      |("FFTFFFTT" (and (not x1) (or (not x0) x2)))
      |("TFTFFFTT" (or (and (not x0) (not x1)) (and x0 x2)))
      |("FTTFFFTT" (and (or (not x0) (or x1 x2)) (or (not x1) (and x0 (not x2)))))
      |("TTTFFFTT" (or (and (not x0) (not x1)) (and x0 (or x1 x2))))
      |("FFFTFFTT" (and (not x1) (or (not x0) (not x2))))
      |("TFFTFFTT" (and (or (not x0) (or x1 (not x2))) (or (not x1) (and x0 x2))))
      |("FTFTFFTT" (or (and (not x0) (not x1)) (and x0 (not x2))))
      |("TTFTFFTT" (or (and (not x0) (not x1)) (and x0 (or x1 (not x2)))))
      |("FFTTFFTT" (not x1))
      |("TFTTFFTT" (or (not x1) (and x0 x2)))
      |("FTTTFFTT" (or (not x1) (and x0 (not x2))))
      |("TTTTFFTT" (or x0 (not x1)))
      |("FFFFTFTT" (and (not x0) (or (not x1) x2)))
      |("TFFFTFTT" (or (and (not x0) (not x1)) (and x1 x2)))
      |("FTFFTFTT" (or (and (not x0) (or (not x1) x2)) (and x0 (and x1 (not x2)))))
      |("TTFFTFTT" (or (and (not x0) (not x1)) (and x1 (or x0 x2))))
      |("FFTFTFTT" (or (and (not x0) (not x1)) (and x2 (or (not x0) (not x1)))))
      |("TFTFTFTT" (or x2 (and (not x0) (not x1))))
      |("FTTFTFTT" (or (and (not x0) (not x1)) (and (or (not x0) (or (not x1) (not x2))) (or x2 (and x0 x1)))))
      |("TTTFTFTT" (or x2 (or (and (not x0) (not x1)) (and x0 x1))))
      |("FFFTTFTT" (and (or (not x0) (not x2)) (or (not x1) x2)))
      |("TFFTTFTT" (or (and (not x1) (not x2)) (and x2 (or (not x0) x1))))
      |("FTFTTFTT" (and (or (not x0) (not x2)) (or x0 (or (not x1) x2))))
      |("TTFTTFTT" (or (and (not x0) (not x1)) (or (and x0 (not x2)) (and x1 x2))))
      |("FFTTTFTT" (or (not x1) (and (not x0) x2)))
      |("TFTTTFTT" (or (not x1) x2))
      |("FTTTTFTT" (or (not x1) (and (or (not x0) (not x2)) (or x0 x2))))
      |("TTTTTFTT" (or x0 (or (not x1) x2)))
      |("FFFFFTTT" (and (not x0) (or (not x1) (not x2))))
      |("TFFFFTTT" (or (and (not x0) (or (not x1) (not x2))) (and x0 (and x1 x2))))
      |("FTFFFTTT" (or (and (not x0) (not x1)) (and x1 (not x2))))
      |("TTFFFTTT" (or (and (not x0) (not x1)) (and x1 (or x0 (not x2)))))
      |("FFTFFTTT" (or (and (not x0) (not x2)) (and (not x1) x2)))
      |("TFTFFTTT" (or (and (not x0) (not x2)) (and x2 (or x0 (not x1)))))
      |("FTTFFTTT" (and (or (not x1) (not x2)) (or (not x0) (or x1 x2))))
      |("TTTFFTTT" (or (and (not x0) (not x1)) (and (or x0 (not x2)) (or x1 x2))))
      |("FFFTFTTT" (or (and (not x0) (not x1)) (and (not x2) (or (not x0) (not x1)))))
      |("TFFTFTTT" (or (and (not x0) (not x1)) (and (or (not x0) (or (not x1) x2)) (or (not x2) (and x0 x1)))))
      |("FTFTFTTT" (or (not x2) (and (not x0) (not x1))))
      |("TTFTFTTT" (or (not x2) (or (and (not x0) (not x1)) (and x0 x1))))
      |("FFTTFTTT" (or (not x1) (and (not x0) (not x2))))
      |("TFTTFTTT" (or (not x1) (or (and (not x0) (not x2)) (and x0 x2))))
      |("FTTTFTTT" (or (not x1) (not x2)))
      |("TTTTFTTT" (or x0 (or (not x1) (not x2))))
      |("FFFFTTTT" (not x0))
      |("TFFFTTTT" (or (not x0) (and x1 x2)))
      |("FTFFTTTT" (or (not x0) (and x1 (not x2))))
      |("TTFFTTTT" (or (not x0) x1))
      |("FFTFTTTT" (or (not x0) (and (not x1) x2)))
      |("TFTFTTTT" (or (not x0) x2))
      |("FTTFTTTT" (or (not x0) (and (or (not x1) (not x2)) (or x1 x2))))
      |("TTTFTTTT" (or (not x0) (or x1 x2)))
      |("FFFTTTTT" (or (not x0) (and (not x1) (not x2))))
      |("TFFTTTTT" (or (not x0) (or (and (not x1) (not x2)) (and x1 x2))))
      |("FTFTTTTT" (or (not x0) (not x2)))
      |("TTFTTTTT" (or (not x0) (or x1 (not x2))))
      |("FFTTTTTT" (or (not x0) (not x1)))
      |("TFTTTTTT" (or (not x0) (or (not x1) x2)))
      |("FTTTTTTT" (or (not x0) (or (not x1) (not x2))))
      |("TTTTTTTT" T)
      |)
      |""".stripMargin

  buildTable()

}
