package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.{Term, propagation}
import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.Term.{Cst, Inter, Union, Var, Elem}
import ca.uwaterloo.flix.util.Result

import scala.util.Random

object TestFastSetUnificationRandom {

  implicit val loc: SourceLocation = SourceLocation.Unknown

  def randomTerm(r: Random, depth: Int, csts: Int, vars: Int, elems: Int): Term = {
    if (csts <= 0 && vars <= 0 && elems <= 0 && depth <= 0) Term.Empty
    else r.nextInt(6) match {
      case 0 if csts <= 0 => randomTerm(r, depth, csts, vars, elems)
      case 0 => Cst(r.nextInt(csts))
      case 1 if vars <= 0 => randomTerm(r, depth, csts, vars, elems)
      case 1 => Var(csts + r.nextInt(vars))
      case 2 if elems <= 0 => randomTerm(r, depth, csts, vars, elems)
      case 2 => Term.Elem(csts + vars + r.nextInt(elems))
      case 3 if depth <= 0 => randomTerm(r, depth, csts, vars, elems)
      case 3 => Term.mkCompl(randomTerm(r, depth - 1, csts, vars, elems))
      case 4 if depth <= 0 => randomTerm(r, depth, csts, vars, elems)
      case 4 => Term.mkInter(List.fill(r.nextInt(4) + 1)(Term.Empty).map(_ => randomTerm(r, depth - 1, csts, vars, elems)))
      case 5 if depth <= 0 => randomTerm(r, depth, csts, vars, elems)
      case 5 => Term.mkUnion(List.fill(r.nextInt(4) + 1)(Term.Empty).map(_ => randomTerm(r, depth - 1, csts, vars, elems)))
    }
  }

  def main4(args: Array[String]): Unit = {
    val t = Cst(1).union(Cst(1).inter(Cst(2).compl().inter(Cst(1))))
    println(t)
    println(FastSetUnification.propagation(t))
  }

  def main(args: Array[String]): Unit = {
    var errs: List[Term] = Nil
    val phases = scala.collection.mutable.Map.empty[Int, Int]
    var continue = true
    var tests = 0
    while (continue) {
      if (tests % 10_000 == 0) println(s"${tests/1000}k")
      tests += 1
      val t = randomTerm(new Random(), 7, 4, 4, 4)
      val (passed, phase) = eqSelf(t)
      if (!passed) {
        continue = false
        errs = Term.mkXor(t, t) :: errs
        phases.updateWith(phase)({
          case None => Some(1)
          case Some(amount) => Some(amount+1)
        })
      }
    }
    println(s"\n\nTests: $tests")
    println(s"Errs: ${errs.size}")
    println(phases)
    errs.sortBy(_.size).headOption.map(_.toRawString).foreach(println)
  }

  def eqSelf(t: Term): (Boolean, Int) = {
    val eq = Term.mkXor(t, t) ~ Term.Empty
    val (res, phase) = FastSetUnification.solveAllInfo(List(eq))
    res match {
      case Result.Ok(subst) => FastSetUnification.verify(subst, List(eq)); (true, phase)
      case Result.Err((ex, _, _)) if ex.isInstanceOf[FastSetUnification.TooComplexException] => (true, phase)
      case Result.Err((ex, _, _)) => (false, phase)
    }
  }

  def example01(): Term = {
    Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(Var(1)), List(Union(Set(), Set(), Set(Var(1)), Set(), Set(), Set(), List(Inter(None, Set(), Set(Var(3), Var(0)), Set(), Set(), Set(Var(2), Var(1)), List()), Inter(None, Set(), Set(), Set(), Set(), Set(Var(1)), List(Union(Set(), Set(), Set(), Set(), Set(), Set(Var(3)), List(Inter(None, Set(), Set(), Set(), Set(), Set(Var(2), Var(1)), List()))), Union(Set(), Set(), Set(Var(1)), Set(), Set(), Set(Var(2), Var(3)), List()))))), Union(Set(), Set(), Set(Var(2), Var(1)), Set(), Set(), Set(Var(3), Var(0)), List()), Union(Set(), Set(), Set(Var(1)), Set(), Set(), Set(), List(Inter(None, Set(), Set(Var(3)), Set(), Set(), Set(), List(Union(Set(), Set(), Set(Var(2), Var(1)), Set(), Set(), Set(), List()))), Inter(None, Set(), Set(Var(2), Var(3)), Set(), Set(), Set(Var(1)), List()))))), Inter(None, Set(), Set(), Set(), Set(), Set(Var(1)), List(Union(Set(), Set(), Set(Var(1)), Set(), Set(), Set(), List(Inter(None, Set(), Set(Var(3), Var(0)), Set(), Set(), Set(Var(2), Var(1)), List()), Inter(None, Set(), Set(), Set(), Set(), Set(Var(1)), List(Union(Set(), Set(), Set(), Set(), Set(), Set(Var(3)), List(Inter(None, Set(), Set(), Set(), Set(), Set(Var(2), Var(1)), List()))), Union(Set(), Set(), Set(Var(1)), Set(), Set(), Set(Var(2), Var(3)), List()))))), Union(Set(), Set(), Set(Var(2), Var(1)), Set(), Set(), Set(Var(3), Var(0)), List()), Union(Set(), Set(), Set(Var(1)), Set(), Set(), Set(), List(Inter(None, Set(), Set(Var(3)), Set(), Set(), Set(), List(Union(Set(), Set(), Set(Var(2), Var(1)), Set(), Set(), Set(), List()))), Inter(None, Set(), Set(Var(2), Var(3)), Set(), Set(), Set(Var(1)), List())))))))
  }

  def runTerm(t: Term): Unit = {
    val (res, phase) = FastSetUnification.solveAllInfo(List(t ~ Term.Empty))
    println(phase)
    res match {
      case Result.Ok(_) => println("good")
      case Result.Err(e) => println("bad")
      println(e)
    }
  }

  def main2(args: Array[String]): Unit = {
    runTerm(example01())
  }

//  def example02(): Term = {
//    Union(Set(), Set(Cst(2)), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(Cst(11)), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(2)), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(Var(8), Var(24)), Set(), Set(), Set(), List()))))), Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(Var(8)), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(Cst(11)), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(2)), Set(Var(24)), List()))), Inter(None, Set(), Set(), Set(), Set(Cst(2), Cst(11)), Set(), List()))))), Inter(None, Set(), Set(), Set(), Set(Cst(2)), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(11)), Set(Var(23)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(11)), Set(Var(24)), List()), Union(Set(), Set(), Set(Var(23), Var(24)), Set(), Set(Cst(11)), Set(), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(11)), Set(Var(19)), List()))))))), Inter(None, Set(), Set(Var(6)), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(Cst(2), Cst(11)), Set(), List()), Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(Var(8)), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(Cst(11)), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(2)), Set(Var(24)), Set(), Set(), Set(), List()))), Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))))), Union(Set(), Set(Cst(2)), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(11)), Set(Var(23)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(11)), Set(Var(24)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(11)), Set(), Set(), Set(), Set(Var(23), Var(24)), List()), Inter(None, Set(Cst(11)), Set(Var(19)), Set(), Set(), Set(), List()))))))))))), Union(Set(), Set(Cst(2)), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(Var(23)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))), Inter(None, Set(), Set(Var(24)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))), Inter(None, Set(Cst(11)), Set(), Set(), Set(Cst(2)), Set(Var(23), Var(24)), List()), Inter(None, Set(), Set(Var(19)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))))))), Inter(None, Set(), Set(Var(24)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()), Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(Var(8)), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(Cst(11)), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(2)), Set(Var(24)), List()))), Inter(None, Set(), Set(), Set(), Set(Cst(2), Cst(11)), Set(), List()))))), Inter(None, Set(), Set(), Set(), Set(Cst(2)), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(Var(23)), List(Inter(None, Set(), Set(), Set(), Set(Cst(2), Cst(11)), Set(), List()))), Union(Set(), Set(), Set(), Set(), Set(), Set(Var(24)), List(Inter(None, Set(), Set(), Set(), Set(Cst(2), Cst(11)), Set(), List()))), Union(Set(), Set(Cst(2)), Set(Var(23), Var(24)), Set(), Set(Cst(11)), Set(), List()), Union(Set(), Set(), Set(), Set(), Set(), Set(Var(19)), List(Inter(None, Set(), Set(), Set(), Set(Cst(2), Cst(11)), Set(), List()))))))))), Inter(None, Set(), Set(Var(23)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))), Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(Var(8)), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(Cst(11)), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(2)), Set(Var(24)), Set(), Set(), Set(), List()))), Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))))), Union(Set(), Set(Cst(2)), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(Var(23)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))), Inter(None, Set(), Set(Var(24)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))), Inter(None, Set(Cst(11)), Set(), Set(), Set(Cst(2)), Set(Var(23), Var(24)), List()), Inter(None, Set(), Set(Var(19)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))))))), Inter(None, Set(), Set(Var(23)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))), Inter(None, Set(), Set(Var(24)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List()))), Inter(None, Set(Cst(11)), Set(), Set(), Set(Cst(2)), Set(Var(23), Var(24)), List()), Inter(None, Set(), Set(Var(19)), Set(), Set(), Set(), List(Union(Set(), Set(Cst(2), Cst(11)), Set(), Set(), Set(), Set(), List())))))
//  }

  def main54(args: Array[String]): Unit = {
    println(example01())
    println(propagation(example01()))
  }

//  def example03(): Term = {
//    Union(Set(), Set(Cst(10)), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(13)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(), Set(), Set(), Set(), List(Union(Set(), Set(Cst(10)), Set(Var(21)), Set(), Set(), Set(), List()))), Inter(None, Set(Cst(10)), Set(), Set(), Set(), Set(), List(Union(Set(), Set(Cst(10)), Set(Var(9)), Set(), Set(), Set(), List()))), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()))), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(), Set(Var(15)), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))), Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))))), Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))), Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))))))), Union(Set(), Set(), Set(), Set(), Set(), Set(Var(20)), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()))), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))))))))))))), Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()))), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()))), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))))))), Inter(None, Set(), Set(Var(20)), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))), Union(Set(), Set(), Set(), Set(), Set(), Set(), List(Inter(None, Set(), Set(), Set(), Set(), Set(), List(Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List()), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(38)), List()))), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(), List()))))))))))))), Inter(None, Set(Cst(10)), Set(Var(23), Var(38)), Set(), Set(), Set(), List()), Inter(None, Set(Cst(10)), Set(Var(38)), Set(), Set(), Set(Var(23)), List()))), Union(Set(), Set(), Set(), Set(), Set(Cst(10)), Set(Var(23), Var(38)), List()), Union(Set(), Set(), Set(Var(23)), Set(), Set(Cst(10)), Set(Var(38)), List())))))))))
//  }

}

