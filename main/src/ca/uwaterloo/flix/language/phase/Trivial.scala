/*
 *  Copyright 2019 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expression, FormalParam, Root}
import ca.uwaterloo.flix.language.ast.{Ast, BinaryOperator, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError.TrivialExpression
import ca.uwaterloo.flix.language.phase.Redundancy.Used
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

// TODO: Come up with better name.
object Trivial extends Phase[TypedAst.Root, TypedAst.Root] {

  // TODO: Introduce annotated expression, e.g. @trivial 0 + 0
  // TODO: Introduce annotated expression: @unreachable 2 + 1, or 2 + 3 @ dead.

  // TODO: Introduce custom error type.
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, RedundancyError] = flix.phase("Trivial") {

    // TODO: Introduce flag to disable

    // Check for trivial expressions.
    val trivial: Used = root.defs.par.aggregate(Used.Neutral)({
      case (acc, (sym, decl)) => acc ++ visitDef(decl)(root, flix)
    }, _ ++ _)

    // TODO: Actually return these errors.

    root.toSuccess
  }

  object Catalog {

    val TInt32: Type = Type.Cst(TypeConstructor.Int32)

    import Expression._
    import SourceLocation.{Unknown => SL}
    import ast.Eff.Pure

    val Unit: Expression = Expression.Unit(SL)
    val Wild: Expression = Expression.Wild(TInt32, Pure, SL)
    val Zer: Expression = Expression.Int32(0, SL)
    val One: Expression = Expression.Int32(1, SL)
    val EmptyString: Expression = Expression.Str("", SL)
    val ListNil: Expression = Expression.Tag(Symbol.mkEnumSym("List"), "Nil", Unit, TInt32, Pure, SL)

    def mkVar()(implicit flix: Flix): Expression = Expression.Var(Symbol.freshVarSym()(flix.genSym), TInt32, Pure, SL)

    def add(e1: Expression, e2: Expression): Expression =
      Binary(BinaryOperator.Plus, e1, e2, TInt32, Pure, SL)

    def sub(e1: Expression, e2: Expression): Expression =
      Binary(BinaryOperator.Minus, e1, e2, TInt32, Pure, SL)

    def mul(e1: Expression, e2: Expression): Expression =
      Binary(BinaryOperator.Times, e1, e2, TInt32, Pure, SL)

    def div(e1: Expression, e2: Expression): Expression =
      Binary(BinaryOperator.Divide, e1, e2, TInt32, Pure, SL)

    def app(e1: Expression, e2: Expression): Expression = Apply(e1, e2, TInt32, Pure, SL)

    def tag(s: String, t: String, e: Expression): Expression = {
      val sym = Symbol.mkEnumSym(s)
      Tag(sym, t, e, TInt32, Pure, SL)
    }

    def defn(s: String): Expression = {
      val sym = Symbol.mkDefnSym(s)
      Expression.Def(sym, TInt32, Pure, SL)
    }

    def identity()(implicit flix: Flix): Expression = {
      val sym = Symbol.freshVarSym()(flix.genSym)
      val fparam = FormalParam(sym, Ast.Modifiers.Empty, TInt32, SL)
      val body = Var(sym, TInt32, Pure, SL)
      Lambda(fparam, body, TInt32, Pure, SL)
    }

    /**
      * Trivial Expression: _ + 0
      */
    def leftAdditionByZero(): Expression = add(Zer, Wild)

    /**
      * Trivial Expression: 0 + _
      */
    def rightAdditionByZero(): Expression = add(Wild, Zer)

    /**
      * Trivial Expression: _ - 0
      */
    def subtractionByZero(): Expression = sub(Wild, Zer)

    /**
      * Trivial Expression: x - x
      */
    def subtractionBySelf()(implicit flix: Flix): Expression = {
      val varX = mkVar()
      sub(varX, varX)
    }

    /**
      * Trivial Expression: 0 * _
      */
    def leftMultiplicationByZero(): Expression = mul(Zer, Wild)

    /**
      * Trivial Expression: _ * 0
      */
    def rightMultiplicationByZero(): Expression = mul(Wild, Zer)

    /**
      * Trivial Expression: 1 * _
      */
    def leftMultiplicationByOne(): Expression = mul(One, Wild)

    /**
      * Trivial Expression: _ * 1
      */
    def rightMultiplicationByOne(): Expression = mul(Wild, One)

    /**
      * Trivial Expression: _ / 1
      */
    def divisionByOne(): Expression = div(Wild, One)

    /**
      * Trivial Expression: x / x
      */
    def divisionBySelf()(implicit flix: Flix): Expression = {
      val varX = mkVar()
      div(varX, varX)
    }

    /**
      * Trivial Expression: "" + _
      */
    def leftConcatenateEmptyString()(implicit flix: Flix): Expression = add(EmptyString, Wild)

    /**
      * Trivial Expression: _ + ""
      */
    def rightConcatenateEmptyString()(implicit flix: Flix): Expression = add(Wild, EmptyString)

    /**
      * Trivial Expression: Nil ::: _
      */
    def leftAppendNil()(implicit flix: Flix): Expression = app(app(defn("List.append"), ListNil), Wild)

    /**
      * Trivial Expression: _ ::: Nil
      */
    def rightAppendNil()(implicit flix: Flix): Expression = app(app(defn("List.append"), Wild), ListNil)

    /**
      * Trivial Expression: List.isEmpty(_ :: _)
      */
    def listIsEmptyCons()(implicit flix: Flix): Expression = app(defn("List.isEmpty"), tag("List", "Cons", Wild))

    /**
      * Trivial Expression: List.map(x -> x, _)
      */
    def listMapIdentity()(implicit flix: Flix): Expression = app(app(defn("List.map"), identity()), Wild) // TODO: Express more concisely.

    // TODO: Use cases to find:

    // TODO: - List.getWithDefault(List.map(_, o), false)         --> List.exists(_)
    // TODO: - List.isEmpty(xs) && List.exists(_, xs)             --> false
    // TODO: - Option.flatMap(x => if (f(x)) Some(x) else None))  --> Option.filter(f)

    // TODO: Probably need to introduce something like Theorem which holds the result too... And then the error can show the result.
    // TODO: DOC
    def allPatterns(implicit root: Root, flix: Flix): List[Expression] = List(
      rightAdditionByZero(),
      leftAdditionByZero(),
      subtractionByZero(),
      //subtractionBySelf(), // TODO
      leftMultiplicationByZero(),
      rightMultiplicationByZero(),
      leftMultiplicationByOne(),
      rightMultiplicationByOne(),
      divisionByOne(),
      // divisionBySelf(), // TODO
      leftConcatenateEmptyString(),
      rightConcatenateEmptyString(),
      leftAppendNil(),
      rightAppendNil(),
      listIsEmptyCons(),
      listMapIdentity()
    )

  }

  // TODO: DOC
  private def visitDef(defn: Def)(implicit root: Root, flix: Flix): Used = {
    visitExp(defn.exp)
  }

  // TODO: DOC
  private def visitExp(exp0: Expression)(implicit root: Root, flix: Flix): Used = checkTrivial(exp0) ++ (exp0 match {

    // TODO: Should not call checkTrivial, just recurse.

    case Expression.Unit(_) => Used.Neutral

    case Expression.True(_) => Used.Neutral

    case Expression.False(_) => Used.Neutral

    case Expression.Char(_, _) => Used.Neutral

    case Expression.Float32(_, _) => Used.Neutral

    case Expression.Float64(_, _) => Used.Neutral

    case Expression.Int8(_, _) => Used.Neutral

    case Expression.Int16(_, _) => Used.Neutral

    case Expression.Int32(_, _) => Used.Neutral

    case Expression.Int64(_, _) => Used.Neutral

    case Expression.BigInt(_, _) => Used.Neutral

    case Expression.Str(_, _) => Used.Neutral

    case Expression.Wild(_, _, _) => Used.Neutral

    case Expression.Var(_, _, _, _) => Used.Neutral

    case Expression.Def(_, _, _, _) => Used.Neutral

    case Expression.Eff(_, _, _, _) => Used.Neutral

    case Expression.Hole(_, _, _, _) => Used.Neutral

    case Expression.Lambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Apply(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRec(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(exp, rules, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.Switch(rules, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.Tuple(elms, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.RecordEmpty(_, _, _) => Used.Neutral

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.ArrayLit(elms, _, _, _) =>
      elms.foldLeft(Used.Neutral) {
        case (acc, e) => acc ++ visitExp(e)
      }

    case Expression.ArrayNew(elm, len, _, _, _) =>
      visitExp(elm) ++ visitExp(len)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.ArrayLength(base, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.VectorLit(elms, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.VectorNew(elm, len, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.VectorLoad(base, index, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.VectorLength(base, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.Ref(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.Deref(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.Assign(exp1, exp2, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.HandleWith(exp, bindings, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.Existential(fparam, exp, eff, loc) => Used.Neutral // TODO

    case Expression.Universal(fparam, exp, eff, loc) => Used.Neutral // TODO

    case Expression.Ascribe(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.Cast(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.TryCatch(exp, rules, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.NativeField(field, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.NativeMethod(method, args, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.NewChannel(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.GetChannel(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.SelectChannel(rules, default, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.ProcessSpawn(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.ProcessSleep(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.ProcessPanic(msg, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.FixpointConstraint(c, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.FixpointSolve(exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => Used.Neutral // TODO

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => Used.Neutral // TODO
  })

  // TODO: Recursively check for these.
  private def checkTrivial(e0: TypedAst.Expression)(implicit root: Root, flix: Flix): Used =
    Catalog.allPatterns.foldLeft(Used.Neutral) {
      case (acc, x) if unify(e0, x).nonEmpty => acc + TrivialExpression(e0.loc)
      case (acc, x) => acc
    }

  // TODO: Return ExpSubstitiotion
  private def unify(e1: Expression, e2: Expression): Option[Unit] = (e1, e2) match {


    // TODO: Order

    case (Expression.Wild(_, _, _), _) => Some(())

    case (_, Expression.Wild(_, _, _)) => Some(())

    case (Expression.Var(sym1, _, _, _), Expression.Var(sym2, _, _, _)) =>
      if (sym1 != sym2)
        Some(()) // TODO
      else
        Some(())

    case (Expression.Def(sym1, _, _, _), Expression.Def(sym2, _, _, _)) =>
      if (sym1 != sym2)
        None
      else
        Some(())

    case (Expression.Unit(_), Expression.Unit(_)) => Some(())

    case (Expression.Str(lit1, _), Expression.Str(lit2, _)) =>
      if (lit1 != lit2)
        None
      else
        Some(())

    case (Expression.Int32(lit1, _), Expression.Int32(lit2, _)) =>
      if (lit1 != lit2)
        None
      else
        Some(())

    case (Expression.Binary(op1, exp11, exp12, _, _, _), Expression.Binary(op2, exp21, exp22, _, _, _)) =>
      if (op1 != op2)
        None
      else
        for {
          subst1 <- unify(exp11, exp21)
          subst2 <- unify(exp12, exp22)
        } yield ()

    case (Expression.Lambda(fparam1, exp1, _, _, _), Expression.Lambda(fparam2, exp2, _, _, _)) =>
      // TODO: Here we should subst.
      for {
        subst1 <- unify(exp1, exp2)
      } yield ()

    case (Expression.Apply(exp11, exp12, _, _, _), Expression.Apply(exp21, exp22, _, _, _)) =>
      for {
        subst1 <- unify(exp11, exp21)
        subst2 <- unify(exp12, exp22)
      } yield ()

    case (Expression.Tag(sym1, tag1, exp1, _, _, _), Expression.Tag(sym2, tag2, exp2, _, _, _)) =>
      if (sym1 != sym2 || tag1 != tag2)
        None
      else
        for {
          subst <- unify(exp1, exp2)
        } yield ()

    // TODO: Add remaining cases

    case _ => None
  }


  /////////////////////////////////////////////////////////////////////////////
  // TODOs
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Compile to automaton or similar?

  // TODO: Should we also consider tricky cases such as:
  // match s with {
  // case Circle(Red) =>
  // case Circle(x) =>
  // where we know that x cannot be red, because that would have matched?
  // What about nested patterns like:
  // def main(): Int =
  //    let s = Circle(Red);
  //    match s with {
  //        case Circle(Red) => 123
  //        case Square(Blu) => match s with {
  //            case Square(Red) =>
  //        }
  //        case _ => Square(Blu)
  //    }

  // TODO: Write argument about dynamic checks/assertions and dead code.

  // TODO: Add while(true) java case to paper?

  // TODO: Ensure everything is private.

  /////////////////////////////////////////////////////////////////////////////
  // Paper Notes
  /////////////////////////////////////////////////////////////////////////////

  // Papers:
  // - Finding Application Errors and Security Flaws Using PQL: a Program Query Language
  // - Using SCL to Specify and Check Design Intent in Source Code
  // - A Framework for Source Code Search using Program Patterns

  // Notes for the paper:
  // - We disallow shadowing (because its confusing in the presence of pattern matching).
  // - We disallow both implicit widening and narrowing of integers.
  // - We disallow all forms of implicit coercions.
  // - We disallow linear patterns.
  // - We treat holes (and ???) as using all local variables (but not anything else?)
  // - We implement the checker using a fork-join style monoid thingy.
  // - If we allow shadowing then that might lead to "mysterious" unused variable warnings.

  // Questions:
  // - When is an enum used? Is it enough to (a) mention its type, (b) to use it in a pat match, or (c) to actually construct a value.
  //     (What if you match on a value of that type, but use a wildcard?)
  //     (What is consistent with the Void enum and the singleton enum?)
  // - When is a predicate used? Is it enough to use it in a rule, or must it also appear in a head predicate?
  // - How do we appropriately distinguish between the effect of NewChannel and e.g. PutChannel?
  //     (How do we deal with return values that must be used, e.g. deleteFile?)

  // Bugs found:
  // - Missing @test on def testArrayLength42(): Int = let x = [[1 :: Nil], [3 :: 4 :: 5 :: Nil]]; length[x]

  // Shadowing gone wrong:
  //     case Expression.FixpointProject(pred, exp, _, _, _) =>
  //      val PredicateWithParam(sym, exp) = pred
  //      mapN(visitExp(pred.exp, env0), visitExp(exp, env0)) {
  //        case (used1, used2) => Used.of(sym) ++ used1 ++ used2
  //      }

  // Shadowing in action:
  // let childList : List[Path] = Nil;
  // let childList = childrenHelper(dirIterator, childList);

  // Count impacted test cases?

  // thm \forall f: List.filter(f, Nil) = Nil
  // false <= List.isEmpty(xs), List.nonEmpty(xs).

  // Ideas from: Using Redundancies to Find Errors

  // [Idempotent operations]: (1) Assign to self, (2) divide by itself, (3) bitwise xord, (4) bitwise and,
  // (Assignment to self could account for record update), there are also refs.

  // [Redundant Assignments]

  // [Dead Code] (early returns, so not really relevant).

  // [Redundant Conditionals]: Detects branches that are always dead.
  // Implemented as a combination of (1) integer propagation, (2) set of known predicates, and (3) bounds on integers.

  // Compile the theorems/bugpatterns to an automaton. Union could be fast.

}
