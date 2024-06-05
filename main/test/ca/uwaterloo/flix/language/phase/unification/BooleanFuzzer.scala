/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.BooleanFuzzer.RawString.toRawString
import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.Term
import ca.uwaterloo.flix.util.Result

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object BooleanFuzzer {

  private implicit val loc: SourceLocation = SourceLocation.Unknown

  /**
    * Contains the constructors of a boolean algebra [[T]].
    */
  case class FormulaFormer[T](
                               top: T,
                               bot: T,
                               cst: Int => T,
                               varr: Int => T,
                               elem: Int => T,
                               union: List[T] => T,
                               inter: List[T] => T,
                               compl: T => T
                             )

  /**
    * The former of [[Term]].
    */
  def termFormer(): FormulaFormer[Term] = FormulaFormer(
    Term.Univ,
    Term.Empty,
    Term.Cst,
    Term.Var,
    Term.mkElemSet(_: Int),
    Term.mkUnion(_: List[Term]),
    Term.mkInter(_: List[Term]),
    Term.mkCompl
  )

  /**
    * Constructs a random boolean algebra formula.
    *
    * @param former constructors for the boolean algebra
    * @param r object for random choice
    * @param depth the maximum depth of the formula
    * @param csts the size of the universe of constants, e.g. 0 means no constants
    * @param vars the size of the universe of variables, e.g. 0 means no variables
    * @param elems the size of the universe of elements, e.g. 0 means no elements
    * @tparam T the type of the underlying formula
    * @return the random formula
    */
  def randomTerm[T](former: FormulaFormer[T], r: Random, depth: Int, csts: Int, vars: Int, elems: Int): T = {
    if (csts <= 0 && vars <= 0 && elems <= 0 && depth <= 0) former.bot
    else r.nextInt(6) match {
      case 0 if csts <= 0 => randomTerm(former, r, depth, csts, vars, elems)
      case 0 => former.cst(r.nextInt(csts))
      case 1 if vars <= 0 => randomTerm(former, r, depth, csts, vars, elems)
      case 1 => former.varr(csts + r.nextInt(vars))
      case 2 if elems <= 0 => randomTerm(former, r, depth, csts, vars, elems)
      case 2 => former.elem(csts + vars + r.nextInt(elems))
      case 3 if depth <= 0 => randomTerm(former, r, depth, csts, vars, elems)
      case 3 => former.compl(randomTerm(former, r, depth - 1, csts, vars, elems))
      case 4 if depth <= 0 => randomTerm(former, r, depth, csts, vars, elems)
      case 4 => former.inter(List.fill(r.nextInt(4) + 1)(randomTerm(former, r, depth - 1, csts, vars, elems)))
      case 5 if depth <= 0 => randomTerm(former, r, depth, csts, vars, elems)
      case 5 => former.union(List.fill(r.nextInt(4) + 1)(randomTerm(former, r, depth - 1, csts, vars, elems)))
    }
  }

  def main(args: Array[String]): Unit = {
    fuzz(new Random(), 2000_000, 5, -1)
  }

  def fuzz(random: Random, testLimit: Int, errLimit: Int, timeoutLimit: Int): Boolean = {
    val former = termFormer()
    val errs: ListBuffer[Term] = ListBuffer.empty
    val errPhaseFrequence = mutable.Map.empty[Int, Int]
    val timeouts: ListBuffer[Term] = ListBuffer.empty
    val timeoutPhaseFrequence = mutable.Map.empty[Int, Int]
    var continue = true
    var tests = 0
    while (continue && tests < testLimit) {
      if (tests % 10_000 == 0) {
        val errAmount = errs.length
        val timeoutAmount = timeouts.length
        println(s"${tests/1000}k (${(tests-errAmount-timeoutAmount)/1000}k, ${errAmount} errs, ${timeoutAmount} t.o.)")
      }
      tests += 1
      val t = randomTerm(former, random, 8, 6, 6, 6)
      val (res, phase) = eqSelf(t)
      res match {
        case Res.Pass => ()
        case Res.Fail =>
          errs += Term.mkXor(t, t)
          inc(errPhaseFrequence, phase)
          if (errLimit > 0 && errs.sizeIs >= errLimit) continue = false
        case Res.Timeout =>
          timeouts += Term.mkXor(t, t)
          inc(timeoutPhaseFrequence, phase)
          if (timeoutLimit > 0 && timeouts.sizeIs >= timeoutLimit) continue = false
      }
    }
    println()
    println(s"   Tests: $tests")
    val errSize = errs.size
    println(s"    Errs: $errSize (${errSize/(1.0*tests) * 100} %)")
    val timeoutSize = timeouts.size
    println(s"Timeouts: $timeoutSize (${timeoutSize/(1.0*tests) * 100} %)")
    if (errPhaseFrequence.nonEmpty) println(s"Err phases:")
    errPhaseFrequence.toList.sorted.foreach(p => println(s"\t\tphase ${p._1}: ${p._2} errors"))
    if (timeoutPhaseFrequence.nonEmpty) println(s"Timeout phases:")
    timeoutPhaseFrequence.toList.sorted.foreach(p => println(s"\t\tphase ${p._1}: ${p._2} timeouts"))
    if (errs.nonEmpty) println(s"Smallest error:")
    errs.sortBy(_.size).headOption.foreach(err => println(s"> ${err.toString}\n> ${toRawString(err)}"))
    if (timeouts.nonEmpty) println(s"Smallest timeout:")
    timeouts.sortBy(_.size).headOption.foreach(timeout => println(s"> ${timeout.toString}\n> ${toRawString(timeout)}"))
    errs.isEmpty
  }

  private def inc[K](m: mutable.Map[K, Int], k: K): Unit = {
    m.updateWith(k)(opt => Some(opt.getOrElse(0) + 1))
  }

  private sealed trait Res {
    def passed: Boolean = this match {
      case Res.Pass => true
      case Res.Fail => false
      case Res.Timeout => false
    }
  }
  private object Res {
    case object Pass extends Res
    case object Fail extends Res
    case object Timeout extends Res
  }

  private def eqSelf(t: Term): (Res, Int) = {
    val eq = Term.mkXor(t, t) ~ Term.Empty
    val (res, phase) = FastSetUnification.solveAllInfo(List(eq))
    res match {
      case Result.Ok(subst) =>
        FastSetUnification.verify(subst, List(eq))
        (Res.Pass, phase)
      case Result.Err((ex, _, _)) if ex.isInstanceOf[FastSetUnification.TooComplexException] =>
        (Res.Timeout, phase)
      case Result.Err((_, _, _)) =>
        (Res.Fail, phase)
    }
  }

  object RawString {
    def toRawString(t: Term): String = t match {
      case Term.Univ => "Univ"
      case Term.Empty => "Empty"
      case Term.Cst(c) => s"Cst($c)"
      case Term.Var(x) => s"Var($x)"
      case Term.ElemSet(i) => s"ElemSet(SortedSet(${i.mkString(", ")}))"
      case Term.Compl(t) => s"Compl(${toRawString(t)})"
      case Term.Inter(posElem, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        val pe = posElem match {
          case Some(value) => s"Some(${toRawString(value)})"
          case None => s"None"
        }
        s"Inter($pe, ${helpSet(posCsts)}, ${helpSet(posVars)}, ${helpSet(negElems)}, ${helpSet(negCsts)}, ${helpSet(negVars)}, ${helpList(rest)})"
      case Term.Union(posElems, posCsts, posVars, negElems, negCsts, negVars, rest) =>
        s"Union(${helpSet(posElems)}, ${helpSet(posCsts)}, ${helpSet(posVars)}, ${helpSet(negElems)}, ${helpSet(negCsts)}, ${helpSet(negVars)}, ${helpList(rest)})"
    }
    private def helpList(l: Iterable[Term]): String = l.map(toRawString).mkString("List(", ", ", ")")
    private def helpSet(l: Iterable[Term]): String = l.map(toRawString).mkString("Set(", ", ", ")")
  }

}
