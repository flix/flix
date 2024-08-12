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
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver
import ca.uwaterloo.flix.language.phase.unification.BooleanPropTesting.RawString.toRawStringEqs
import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.Term._
import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.{Equation, Term}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable.ListBuffer
import scala.util.Random

object BooleanPropTesting {

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
    testSolvableConstraints(new Random(), explodedRandomXor, 500_000, 1, -1)
  }

  // TODO add testing of t ~ propagation(t)

  def testSolvableConstraints(random: Random, genSolvable: Random => List[Equation], testLimit: Int, errLimit: Int, timeoutLimit: Int): Boolean = {
    def printProgress(tests: Int, errAmount: Int, timeoutAmount: Int): Unit = {
      val passed = tests - errAmount - timeoutAmount
      println(s"${tests / 1000}k (${passed} passed, $errAmount errs, $timeoutAmount t.o.)")
    }

    val passes: ListBuffer[Int] = ListBuffer.empty
    // input, verfierError, phase
    val errs: ListBuffer[(List[Equation], Boolean, Int)] = ListBuffer.empty
    val timeouts: ListBuffer[(List[Equation], Int)] = ListBuffer.empty
    var continue = true
    var tests = 0
    var start = System.currentTimeMillis()

    while (continue && (testLimit <= 0 || tests < testLimit)) {
      val now = System.currentTimeMillis()
      if (now - start >= 2000) {
        start = now
        printProgress(tests, errs.length, timeouts.length)
      }
      tests += 1
      val input = genSolvable(random)
      val (res, phase) = runEquations(input)
      res match {
        case Res.Pass =>
          passes += phase
        case Res.Fail(verf) =>
          errs += ((input, verf, phase))
          if (errLimit > 0 && errs.sizeIs >= errLimit) continue = false
        case Res.Timeout =>
          timeouts += ((input, phase))
          if (timeoutLimit > 0 && timeouts.sizeIs >= timeoutLimit) continue = false
      }
    }
    val (smallestError, smallestTimeout) = printTestOutput(errs, timeouts, tests)
    def askAndRun(description: String)(l: List[Equation]) = {
      if (askYesNo(s"Do you want to run $description?")) runEquations(l)(debugging = true)
    }
    smallestError.map(askAndRun("smallest error"))
    smallestTimeout.map(askAndRun("smallest timeout"))
    errs.isEmpty
  }

  private def printTestOutput(errs: ListBuffer[(List[Equation], Boolean, Int)], timeouts: ListBuffer[(List[Equation], Int)], tests: Int): (Option[List[Equation]], Option[List[Equation]]) = {
    println()
    println(s"   Tests: $tests")
    val errSize = errs.size
    val verfSize = errs.count { case (_, b, _) => b }
    println(s"    Errs: $errSize (${errSize / (1.0 * tests) * 100} %) ($verfSize verification errors)")
    val timeoutSize = timeouts.size
    println(s"Timeouts: $timeoutSize (${timeoutSize / (1.0 * tests) * 100} %)")
    if (errs.nonEmpty) println(s"\nSmallest error:")
    val smallestError = errs.sortBy { case (a, _, p) => (p, a.map(_.size).sum) }.headOption
    smallestError.foreach {
      case (err, verf, phase) => println(s">${if (verf) "v" else ""}$phase: ${err.mkString("\n")}\n>${if (verf) "v" else ""}$phase: ${toRawStringEqs(err)}")
    }
    if (timeouts.nonEmpty) println(s"\nSmallest timeout:")
    val smallestTimeout = timeouts.sortBy(p => (p._2, p._1.map(_.size).sum)).headOption
    smallestTimeout.foreach {
      case (timeout, phase) => println(s">$phase: ${timeout.mkString("\n")}\n>$phase: ${toRawStringEqs(timeout)}")
    }
    (smallestError.map(_._1), smallestTimeout.map(_._1))
  }

  def propagationTesting(random: Random): List[Equation] = {
    val former = termFormer()
    val t = randomTerm(former, random, 4, 3, 3, 3)
    List(t ~ FastSetUnification.propagation(t))
  }

  def explodedRandomXor(random: Random): List[Equation] = {
    val former = termFormer()
    val t = randomTerm(former, random, 4, 3, 3, 3)
    groupAssignments(explodeKnownEquation(random, eqPropagatedSelf(t)), 0)
  }

  private sealed trait Res

  private object Res {
    case object Pass extends Res

    case class Fail(verf: Boolean) extends Res

    case object Timeout extends Res
  }

  private def runEquations(eqs: List[Equation])(implicit debugging: Boolean = false): (Res, Int) = {
    val (res, phase) = FastSetUnification.solveAllInfo(eqs)
    res match {
      case Result.Ok(subst) => try {
        FastSetUnification.verify(subst, eqs)
        (Res.Pass, phase)
      } catch {
        case _: InternalCompilerException => (Res.Fail(true), phase)
      }
      case Result.Err((ex, _, _)) if ex.isInstanceOf[FastSetUnification.TooComplexException] =>
        (Res.Timeout, phase)
      case Result.Err((_, _, _)) =>
        (Res.Fail(false), phase)
    }
  }

  /** Defaults to no in case of err */
  private def askYesNo(question: String): Boolean = {
    println(question)
    scala.io.StdIn.readBoolean()
  }

  private def eqPropagatedSelf(t: Term): Equation = {
    t ~ FastSetUnification.propagation(t)
  }

  /** Returns an empty equivalent term */
  private def xorSelf(t: Term): Term = {
    Term.mkXor(t, t)
  }

  /** finds pairs of x1 ~ f and x2 ~ g and collects them into one equation with unions of sides */
  private def groupAssignments(l: List[Equation], maxAmount: Int): List[Equation] = {
    var res: List[Equation] = Nil
    var prev: Option[(Term.Var, Term)] = None
    var collections = 0

    for (eq <- l) {(eq, prev) match {
      case (Equation(x@Term.Var(_), t1, loc), Some((y, t2))) if maxAmount > 0 && collections < maxAmount =>
        collections += 1
        prev = None
        res = Equation.mk(Term.mkUnion(x, y), Term.mkUnion(t1, t2), loc) :: res
      case (Equation(x@Term.Var(_), t, _), None) =>
        prev = Some(x, t)
      case (other, _) => res = other :: res
    }}
    prev.foreach{case (x, t) => res = (x ~ t) :: res}
    res
  }

  /**
    * Takes an equation and creates equations that names some subterms as variables via extra equations.
    */
  private def explodeKnownEquation(r: Random, eq: Equation): List[Equation] = {
    var next = maxId(eq.t1) max maxId(eq.t2)

    def getId(): Int = {
      next += 1
      next
    }

    val (left, leftEqs) = explode(r, eq.t1, eq.loc, getId())
    val (right, rightEqs) = explode(r, eq.t2, eq.loc, getId())
    Equation.mk(left, right, eq.loc) :: leftEqs ++ rightEqs
  }

  private def explode(r: Random, t: Term, loc: SourceLocation, gen: => Int): (Term, List[Equation]) = t match {
    case Term.Univ | Term.Empty | Term.Cst(_) | Term.ElemSet(_) | Term.Var(_) =>
      if (r.nextInt(7) == 0) {
        val fresh = Term.Var(gen)
        (fresh, List(Equation.mk(fresh, t, loc)))
      } else (t, Nil)
    case Term.Compl(t0) =>
      val (t1, eqs) = explode(r, t0, loc, gen)
      if (r.nextInt(5) == 0) {
        val fresh = Term.Var(gen)
        (fresh, Equation.mk(fresh, Term.mkCompl(t1), loc) :: eqs)
      } else (t, eqs)
    case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      splitTerms(Term.mkInter, r, loc, gen, posElem, posCsts, posVars, negElem, negCsts, negVars, rest)
    case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      splitTerms(Term.mkUnion, r, loc, gen, posElem, posCsts, posVars, negElem, negCsts, negVars, rest)
  }

  private def splitTerms(build: List[Term] => Term, r: Random, loc: SourceLocation, gen: => Int, posElem: Option[Term.ElemSet], posCsts: Set[Term.Cst], posVars: Set[Term.Var], negElem: Option[Term.ElemSet], negCsts: Set[Term.Cst], negVars: Set[Term.Var], rest0: List[Term]): (Term, List[Equation]) = {
    var eqs: List[Equation] = Nil
    var rest: List[Term] = Nil
    for (r0 <- rest0) {
      val (rr, eq) = explode(r, r0, loc, gen)
      rest = rr :: rest
      eqs = eq ++ eqs
    }
    val terms0: List[Term] = posElem.toList ++ posCsts.toList ++ posVars.toList ++ negElem.toList.map(Term.mkCompl) ++ negCsts.toList.map(Term.mkCompl) ++ negVars.toList.map(Term.mkCompl) ++ rest
    val terms = r.shuffle(terms0)
    // chose between at least 1 and at most terms - 2 or 4
    val maxRoll = 4 min ((terms.size - 2) max 0)
    val chunkAmount = if (maxRoll <= 0) 1 else r.nextInt(maxRoll) + 1
    // reserve chunkAmount elements to have them non-empty
    var (reserved, remaining) = terms.splitAt(chunkAmount)
    var chunks: List[List[Term]] = Nil
    for (i <- Range(0, chunkAmount)) {
      val (hd, reserved1) = (reserved.head, reserved.drop(1))
      reserved = reserved1
      if (remaining.isEmpty) {
        chunks = List(hd) :: chunks
      } else {
        val index = if (i == chunkAmount - 1) remaining.size else r.nextInt(remaining.size + 1)
        val (mine, remaining1) = remaining.splitAt(index)
        remaining = remaining1
        chunks = (hd :: mine) :: chunks
      }
    }
    assert(reserved.isEmpty)
    assert(remaining.isEmpty, remaining.size)
    var finalTerms: List[Term] = Nil
    for (chunk <- chunks) {
      if (r.nextInt(4) != 0) {
        val fresh = Term.Var(gen)
        eqs = Equation.mk(fresh, build(chunk), loc) :: eqs
        finalTerms = fresh :: finalTerms
      } else {
        finalTerms = chunk ++ finalTerms
      }
    }
    (build(finalTerms), eqs)
  }

  @tailrec
  private def maxId(t: Term): Int = t match {
    case Term.Univ => -1
    case Term.Empty => -1
    case Term.Cst(c) => c
    case Term.Var(x) => x
    case Term.ElemSet(s) => maxId(s)
    case Term.Compl(t) => maxId(t)
    case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      maxId(posElem) max maxId(posCsts) max maxId(posVars) max maxId(negElem) max maxId(negCsts) max maxId(negVars) max maxId(rest)
    case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
      maxId(posElem) max maxId(posCsts) max maxId(posVars) max maxId(negElem) max maxId(negCsts) max maxId(negVars) max maxId(rest)
  }

  private def maxId(l: List[Term]): Int = {
    if (l.isEmpty) -1 else l.map(maxId).max
  }

  private def maxId(s: Set[_ <: Term]): Int = {
    if (s.isEmpty) -1 else s.map(maxId).max
  }

  private def maxId(s: SortedSet[Int]): Int = {
    if (s.isEmpty) -1 else s.max
  }

  private def maxId(o: Option[Term.ElemSet]): Int = {
    o.map(es => maxId(es.s)).getOrElse(-1)
  }

  object RawString {
    def toRawString(t: Term): String = t match {
      case Term.Univ => "Univ"
      case Term.Empty => "Empty"
      case Term.Cst(c) => s"Cst($c)"
      case Term.Var(x) => s"Var($x)"
      case Term.ElemSet(s) if s.sizeIs == 1 => s"mkElemSet(${s.head})"
      case Term.ElemSet(s) => s"ElemSet(SortedSet(${s.mkString(", ")}))"
      case Term.Compl(t) => s"Compl(${toRawString(t)})"
      case Term.Inter(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        s"Inter(${toRawString(posElem)}, ${toRawString(posCsts)}, ${toRawString(posVars)}, ${toRawString(negElem)}, ${toRawString(negCsts)}, ${toRawString(negVars)}, ${toRawString(rest)})"
      case Term.Union(posElem, posCsts, posVars, negElem, negCsts, negVars, rest) =>
        s"Union(${toRawString(posElem)}, ${toRawString(posCsts)}, ${toRawString(posVars)}, ${toRawString(negElem)}, ${toRawString(negCsts)}, ${toRawString(negVars)}, ${toRawString(rest)})"
    }

    private def toRawString(l: List[Term]): String = l.map(toRawString).mkString("List(", ", ", ")")

    private def toRawString(l: Set[_ <: Term]): String = l.map(toRawString).mkString("Set(", ", ", ")")

    private def toRawString(l: Option[Term]): String = l match {
      case Some(value) => s"Some(${toRawString(value)})"
      case None => "None"
    }

    def toRawStringEqs(l: List[Equation]): String = {
      s"List(${l.map(eq => s"${toRawString(eq.t1)} ~ ${toRawString(eq.t2)}").mkString(",\n")})"
    }
  }

  private def testCase(): List[Equation] = {
    List(Union(None, Set(), Set(), None, Set(), Set(), List(Inter(None, Set(), Set(Var(17), Var(15), Var(16)), None, Set(), Set(), List()), Inter(None, Set(), Set(Var(25), Var(24)), None, Set(), Set(), List()))) ~ Empty,
      Var(25) ~ Cst(0),
      Var(24) ~ Inter(Some(ElemSet(SortedSet(6))), Set(), Set(Var(18), Var(23)), None, Set(), Set(), List(Union(None, Set(), Set(Var(20), Var(19)), None, Set(), Set(), List()))),
      Var(23) ~ Union(None, Set(), Set(Var(21)), Option(ElemSet(SortedSet(6))), Set(Cst(0)), Set(), List(Inter(None, Set(), Set(Var(22)), Option(ElemSet(SortedSet(6))), Set(), Set(Var(3), Var(5), Var(4)), List()))),
      Var(22) ~ Compl(Cst(2)),
      Var(21) ~ Inter(None, Set(), Set(), Option(ElemSet(SortedSet(8))), Set(Cst(1)), Set(), List()),
      Var(3) ~ Var(20),
      Var(19) ~ Union(Option(ElemSet(SortedSet(6))), Set(Cst(2)), Set(Var(5), Var(4)), None, Set(), Set(), List()),
      Var(18) ~ Union(Option(ElemSet(SortedSet(8))), Set(Cst(1)), Set(), None, Set(), Set(), List()),
      Var(17) ~ ElemSet(SortedSet(6)),
      Var(16) ~ Inter(None, Set(), Set(Var(9), Var(14), Var(10)), None, Set(), Set(), List()),
      Var(15) ~ Cst(0),
      Var(14) ~ Union(None, Set(), Set(Var(11)), Option(ElemSet(SortedSet(6))), Set(Cst(0)), Set(), List(Inter(None, Set(), Set(Var(12), Var(13)), None, Set(Cst(2)), Set(Var(3), Var(5)), List()))),
      Var(13) ~ Compl(ElemSet(SortedSet(6))),
      Var(12) ~ Compl(Var(4)),
      Var(11) ~ Inter(None, Set(), Set(), Option(ElemSet(SortedSet(8))), Set(Cst(1)), Set(), List()),
      Var(10) ~ Union(Option(ElemSet(SortedSet(6))), Set(Cst(2)), Set(Var(3), Var(5), Var(4)), None, Set(), Set(), List()),
      Var(9) ~ Union(Option(ElemSet(SortedSet(8))), Set(Cst(1)), Set(), None, Set(), Set(), List()))
  }

  private def testCases2(): List[Equation] = {
    List(Var(24) ~ Var(25),
      Var(24) ~ Union(None, Set(), Set(), None, Set(), Set(), List(Inter(None, Set(), Set(Var(15), Var(16)), None, Set(), Set(), List()), Inter(None, Set(), Set(Var(23), Var(22)), None, Set(), Set(), List()))),
      Var(23) ~ Inter(None, Set(), Set(), Some(mkElemSet(8)), Set(Cst(0)), Set(Var(5)), List(Union(Some(mkElemSet(8)), Set(Cst(0)), Set(Var(5), Var(19)), None, Set(), Set(), List()))),
      Var(22) ~ Union(None, Set(), Set(Var(21)), Some(mkElemSet(8)), Set(), Set(Var(3)), List()),
      Var(21) ~ Inter(None, Set(), Set(Var(20)), Some(mkElemSet(6)), Set(), Set(), List(Union(None, Set(), Set(), Some(mkElemSet(8)), Set(Cst(0)), Set(), List()))),
      Var(20) ~ Union(None, Set(), Set(), Some(mkElemSet(8)), Set(), Set(Var(5), Var(4)), List()),
      Var(19) ~ Inter(Some(mkElemSet(8)), Set(), Set(Var(18), Var(3)), None, Set(), Set(), List()),
      Var(18) ~ Union(Some(mkElemSet(6)), Set(), Set(Var(17)), None, Set(), Set(), List(Inter(Some(mkElemSet(8)), Set(), Set(Var(5), Var(4)), None, Set(), Set(), List()))),
      Var(17) ~ Inter(Some(mkElemSet(8)), Set(Cst(0)), Set(), None, Set(), Set(), List()),
      Var(16) ~ Compl(mkElemSet(8)),
      Var(15) ~ Inter(None, Set(), Set(Var(12), Var(14)), None, Set(Cst(0)), Set(Var(5)), List()),
      Var(14) ~ Union(None, Set(), Set(Var(13)), Some(mkElemSet(8)), Set(), Set(Var(3)), List()),
      Var(13) ~ Inter(None, Set(), Set(), Some(mkElemSet(6)), Set(), Set(), List(Union(None, Set(), Set(), Some(mkElemSet(8)), Set(), Set(Var(5), Var(4)), List()), Union(None, Set(), Set(), Some(mkElemSet(8)), Set(Cst(0)), Set(), List()))),
      Var(12) ~ Union(Some(mkElemSet(8)), Set(Cst(0)), Set(Var(5)), None, Set(), Set(), List(Inter(Some(mkElemSet(8)), Set(), Set(Var(11), Var(3)), None, Set(), Set(), List()))),
      Var(11) ~ Union(Some(mkElemSet(6)), Set(), Set(Var(9), Var(10)), None, Set(), Set(), List()),
      Var(10) ~ Inter(Some(mkElemSet(8)), Set(Cst(0)), Set(), None, Set(), Set(), List()),
      Var(9) ~ Inter(Some(mkElemSet(8)), Set(), Set(Var(5), Var(4)), None, Set(), Set(), List()),
      Var(25) ~ Empty)
  }

}
