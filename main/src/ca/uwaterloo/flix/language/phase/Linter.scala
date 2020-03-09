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
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

object Linter extends Phase[TypedAst.Root, TypedAst.Root] {

  def run(root: Root)(implicit flix: Flix): Validation[Root, LinterError] = flix.phase("Linter") {
    // Compute all lints in the AST root.
    val lints = lintsOf(root)

    // Compute a list of all non-lint definitions in the program.
    val defs = nonLintsOf(root)

    // TODO: Debug
    println(s"I found: ${lints.length} to match against ${defs.length} definitions.")


    // Searches for applicable lints.
    val results = ParOps.parMap(visitDef(_, lints), defs)

    // Check if there were any applicable lints.
    results.flatten match {
      case Nil => root.toSuccess
      case xs => Failure(LazyList.from(xs.take(100))) // TODO: Only returns the first 100 instances.
    }
  }

  /**
    * Searches for applicable lints in the given definition `defn0`.
    */
  private def visitDef(defn: Def, lints: List[Lint]): List[LinterError] =
    lints.flatMap(visitExp(defn.exp, _))

  /**
    * Computes whether the given lint `l0` is applicable to the given expression `exp0`.
    */
  private def visitExp(exp0: Expression, lint: Lint): Option[LinterError] = {
    if (lint.sym.name == "leftAdditionByZero") // TODO
    unify(exp0, lint.exp) match {
      case None => None
      case Some(_) => Some(LinterError.Simplify("hello", SourceLocation.Unknown))
    }
    else
    None
  }

  private def unify(exp1: Expression, exp2: Expression): Option[Substitution] = (exp1, exp2) match {
    case (Expression.Unit(_), Expression.Unit(_)) => Some(Substitution.empty)

    case (Expression.True(_), Expression.True(_)) => Some(Substitution.empty)

    case (Expression.False(_), Expression.False(_)) => Some(Substitution.empty)

    case (Expression.Char(lit1, _), Expression.Char(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Float32(lit1, _), Expression.Float32(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Float64(lit1, _), Expression.Float64(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Int8(lit1, _), Expression.Int8(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Int16(lit1, _), Expression.Int16(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Int32(lit1, _), Expression.Int32(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Unary(op1, exp1, _, _, _), Expression.Unary(op2, exp2, _, _, _)) if op1 == op2 =>
      unify(exp1, exp2)

    case (Expression.Binary(op1, exp11, exp12, _, _, _), Expression.Binary(op2, exp21, exp22, _, _, _)) if op1 == op2 =>
      for {
        s1 <- unify(exp11, exp21)
        s2 <- unify(s1(exp12), s1(exp22))
      } yield s2 @@ s1

    case _ => None
  }

  /**
    * Returns all lints in the given AST `root`.
    */
  private def lintsOf(root: Root): List[Lint] = root.defs.foldLeft(Nil: List[Lint]) {
    case (acc, (sym, defn)) if (defn.ann.isLint) => defn.exp match {
      case Expression.Universal(_, exp, _) => Lint(defn.sym, exp) :: acc // TODO
      case _ => Lint(defn.sym, defn.exp) :: acc
    }
    case (acc, (sym, defn)) => acc
  }

  /**
    * Returns all non-lints definitions in the given AST `root`.
    */
  private def nonLintsOf(root: Root): List[Def] = root.defs.foldLeft(Nil: List[Def]) {
    case (acc, (sym, defn)) => if (!defn.ann.isLint) defn :: acc else acc
  }

  case class Lint(sym: Symbol.DefnSym, exp: Expression)

  object Substitution {
    /**
      * Represents the empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)
  }

  case class Substitution(m: Map[Symbol.VarSym, Expression]) {

    /**
      * Returns `true` if `this` is the empty substitution.
      */
    val isEmpty: Boolean = m.isEmpty

    /**
      * Applies the substitution to the expression `exp0`.
      */
    def apply(exp0: Expression): Expression = exp0 match {
      case Expression.Unit(_) => exp0

      case Expression.True(_) => exp0

      case Expression.False(_) => exp0

      case Expression.Char(_, _) => exp0

      case Expression.Float32(_, _) => exp0

      case Expression.Float64(_, _) => exp0

      case Expression.Int8(_, _) => exp0

      case Expression.Int16(_, _) => exp0

      case Expression.Int32(_, _) => exp0

      case Expression.Int64(_, _) => exp0

      case Expression.BigInt(_, _) => exp0

      case Expression.Str(_, _) => exp0

      case Expression.Wild(_, _) => exp0

      case Expression.Var(sym, _, _) => m.get(sym) match {
        case None => exp0
        case Some(e) => e
      }

      case Expression.Def(_, _, _) => exp0

      case Expression.Hole(_, _, _, _) => exp0

      case Expression.Lambda(fparam, exp, tpe, loc) =>
        val f = apply(fparam)
        val e = apply(exp)
        Expression.Lambda(f, e, tpe, loc)

      case Expression.Apply(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Apply(e1, e2, tpe, eff, loc)

      case Expression.Unary(op, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Unary(op, e, tpe, eff, loc)

      case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Binary(op, e1, e2, tpe, eff, loc)

      //        case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO
      //
      //        case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Stm(e1, e2, tpe, eff, loc)

      //        case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO

      case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Tag(sym, tag, e, tpe, eff, loc)

      case Expression.Tuple(elms, tpe, eff, loc) =>
        val es = elms.map(apply)
        Expression.Tuple(es, tpe, eff, loc)

      case Expression.RecordEmpty(_, _) => exp0

      case Expression.RecordSelect(exp, label, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.RecordSelect(e, label, tpe, eff, loc)

      case Expression.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.RecordExtend(label, e1, e2, tpe, eff, loc)

      case Expression.RecordRestrict(label, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.RecordRestrict(label, e, tpe, eff, loc)

      case Expression.ArrayLit(elms, tpe, eff, loc) =>
        val es = elms.map(apply)
        Expression.ArrayLit(es, tpe, eff, loc)

      case Expression.ArrayNew(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.ArrayNew(e1, e2, tpe, eff, loc)

      case Expression.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.ArrayLoad(e1, e2, tpe, eff, loc)

      case Expression.ArrayLength(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.ArrayLength(e, tpe, eff, loc)

      case Expression.ArrayStore(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.ArrayStore(e1, e2, e3, tpe, eff, loc)

      case Expression.ArraySlice(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.ArraySlice(e1, e2, e3, tpe, eff, loc)

      case Expression.VectorLit(elms, tpe, eff, loc) =>
        val es = elms.map(apply)
        Expression.VectorLit(es, tpe, eff, loc)

      case Expression.VectorNew(exp, len, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.VectorNew(e, len, tpe, eff, loc)

      case Expression.VectorLoad(exp, index, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.VectorLoad(e, index, tpe, eff, loc)

      case Expression.VectorStore(exp1, index, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.VectorStore(e1, index, e2, tpe, eff, loc)

      case Expression.VectorLength(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.VectorLength(e, tpe, eff, loc)

      case Expression.VectorSlice(exp, begin, end, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.VectorSlice(e, begin, end, tpe, eff, loc)

      case Expression.Ref(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Ref(e, tpe, eff, loc)

      case Expression.Deref(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Deref(e, tpe, eff, loc)

      case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Assign(e1, e2, tpe, eff, loc)

      //        case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {  TODO
      //          def tpe: Type = Type.Bool
      //
      //          def eff: Type = Type.Pure
      //        }
      //
      //        case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {  TODO
      //          def tpe: Type = Type.Bool
      //
      //          def eff: Type = Type.Pure
      //        }

      case Expression.Ascribe(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Ascribe(e, tpe, eff, loc)

      case Expression.Cast(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Cast(e, tpe, eff, loc)


      //        case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO
      //

      case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
        val as = args.map(apply)
        Expression.InvokeConstructor(constructor, as, tpe, eff, loc)

      case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
        val e = apply(exp)
        val as = args.map(apply)
        Expression.InvokeMethod(method, e, as, tpe, eff, loc)

      case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
        val as = args.map(apply)
        Expression.InvokeStaticMethod(method, as, tpe, eff, loc)

      case Expression.GetField(field, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.GetField(field, e, tpe, eff, loc)

      case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.PutField(field, e1, e2, tpe, eff, loc)

      case Expression.GetStaticField(field, tpe, eff, loc) => exp0

      case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.PutStaticField(field, e, tpe, eff, loc)

      case Expression.NewChannel(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.NewChannel(e, tpe, eff, loc)

      case Expression.GetChannel(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.GetChannel(e, tpe, eff, loc)

      case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.PutChannel(e1, e2, tpe, eff, loc)

      //        case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO
      //

      case Expression.ProcessSpawn(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.ProcessSpawn(e, tpe, eff, loc)

      case Expression.ProcessPanic(msg, tpe, eff, loc) => exp0

      //        case class FixpointConstraintSet(cs: List[TypedAst.Constraint], tpe: Type, loc: SourceLocation) extends TypedAst.Expression {  TODO
      //          def eff: Type = Type.Pure
      //        }
      //

      case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.FixpointCompose(e1, e2, tpe, eff, loc)

      case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.FixpointSolve(e, stf, tpe, eff, loc)

      case Expression.FixpointProject(sym, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.FixpointProject(sym, e, tpe, eff, loc)

      case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.FixpointEntails(e1, e2, tpe, eff, loc)

      case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.FixpointFold(sym, e1, e2, e3, tpe, eff, loc)

      case _ => ???

    }

    /**
      * Applies the substitution to the formal parameter `fparam0`.
      */
    def apply(fparam0: FormalParam): FormalParam = fparam0 match {
      case FormalParam(sym, mod, tpe, loc) => m.get(sym) match {
        case None => fparam0
        case Some(otherSym) => FormalParam(???, mod, tpe, loc) // TODO: The subst. need to contain more than just expressions?
      }
    }

    /**
      * Returns the left-biased composition of `this` substitution with `that` substitution.
      */
    def ++(that: Substitution): Substitution = {
      if (this.isEmpty) {
        that
      } else if (that.isEmpty) {
        this
      } else {
        Substitution(
          this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
        )
      }
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution.
      */
    def @@(that: Substitution): Substitution = {
      if (this.isEmpty) {
        that
      } else if (that.isEmpty) {
        this
      } else {
        val newMap = that.m.foldLeft(Map.empty[Symbol.VarSym, Expression]) {
          case (macc, (x, t)) => macc.updated(x, this.apply(t))
        }
        Substitution(newMap) ++ this
      }
    }

  }

}
