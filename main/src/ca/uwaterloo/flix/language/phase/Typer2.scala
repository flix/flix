/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

object Typer2 {

  /**
    * A substitution is a map from type variables to types.
    */
  object Substitution {
    /**
      * Returns the empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)

    /**
      * Returns the singleton substitution mapping `x` to `tpe`.
      */
    def singleton(x: Type.Var, tpe: Type): Substitution = Substitution(Map(x -> tpe))
  }

  case class Substitution(m: Map[Type.Var, Type]) {

    /**
      * Applies `this` substitution to the given type `tpe`.
      */
    def apply(tpe: Type): Type = tpe match {
      case x: Type.Var =>
        m.get(x) match {
          case None => x
          case Some(y) if x.kind == tpe.kind => y
          case Some(y) if x.kind != tpe.kind => throw InternalCompilerException(s"Expected kind `${x.kind}' but got `${tpe.kind}'.")
        }
      case Type.Unit => Type.Unit
      case Type.Bool => Type.Bool
      case Type.Char => Type.Char
      case Type.Float32 => Type.Float32
      case Type.Float64 => Type.Float64
      case Type.Int8 => Type.Int8
      case Type.Int16 => Type.Int16
      case Type.Int32 => Type.Int32
      case Type.Int64 => Type.Int64
      case Type.BigInt => Type.BigInt
      case Type.Str => Type.Str
      case Type.Native => Type.Native
      case Type.Arrow => Type.Arrow
      case Type.FTuple(l) => Type.FTuple(l)
      case Type.FOpt => Type.FOpt
      case Type.FList => Type.FList
      case Type.FVec => Type.FVec
      case Type.FSet => Type.FSet
      case Type.FMap => Type.FMap
      case Type.Enum(name, cases) => Type.Enum(name, cases.map(x => (x._1, apply(x._2).asInstanceOf[Type.Tag])))
      case Type.Apply(t1, t2) => Type.Apply(apply(t1), apply(t2))
      case _ => throw InternalCompilerException(s"Unexpected type: `$tpe'")
    }

    /**
      * Applies `this` substitution to the given types `ts`.
      */
    def apply(ts: List[Type]): List[Type] = ts.map(t => apply(t))

    /**
      * Returns the left-biased  composition of `this` substitution with `that` substitution.
      */
    def ++(that: Substitution): Substitution = {
      Substitution(this.m ++ that.m.filter(kv => !this.m.contains(kv._1)))
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution.
      */
    def @@(that: Substitution): Substitution = {
      val m = that.m.map {
        case (x, t) => x -> this.apply(t)
      }
      Substitution(m) ++ this
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution,
      * provided that the two substitutions are equal on shared variables.
      */
    def merge(that: Substitution): Validation[Substitution, TypeError] = {
      val shared = this.m.keySet intersect that.m.keySet
      val conflicts = shared.filter(v => this.apply(v) != that.apply(v))
      if (conflicts.nonEmpty)
        TypeError.MergeError().toFailure
      else
        (this ++ that).toSuccess
    }

  }


  sealed trait TypeClass

  object TypeClass {

    case object Equal extends TypeClass

    case object Number extends TypeClass

    case object TotalOrder extends TypeClass

    case object Indexed extends TypeClass

  }

  type ContextAndType = (List[(TypeClass, Type)], Type)

  val EmptyContext = Nil

  /**
    * A common super-type for type constraints.
    */
  sealed trait TypeConstraint

  object TypeConstraint {

    // TODO: It is possible that the contexts should be merged,
    // i.e. the signature should be (Context, Type, Type).
    case class Eq(ct1: ContextAndType, ct2: ContextAndType) extends TypeConstraint

  }

  def typer(program: NamedAst.Program)(implicit genSym: GenSym): Unit = {
    for ((ns, defns) <- program.definitions) {
      for ((name, defn) <- defns) {
        ConstraintGeneration.Expressions.gen(defn.exp)
      }
    }
  }

  /**
    * Phase 1: Constraint Generation.
    */
  object ConstraintGeneration {

    object Expressions {

      /**
        * Generates type constraints for the given expression `exp0`.
        */
      def gen(exp0: NamedAst.Expression)(implicit genSym: GenSym): Unit = {

        /*
         * A mutable set to hold the collected constraints.
         */
        val constraints = mutable.Set.empty[TypeConstraint]

        def ret(id: Int, ctx: List[(TypeClass, Type)], tpe: Type): ContextAndType = {
          // Add to constraints
          ???
        }

        /**
          * Generates type constraints for the given expression `e0` under the given type environment `tenv`.
          */
        def visitExp(e0: NamedAst.Expression, tenv0: Map[Symbol.VarSym, Type]): ContextAndType = e0 match {

          /*
           * Wildcard expression.
           */
          case NamedAst.Expression.Wild(id, loc) =>
            ??? // TODO

          /*
           * Variable expression.
           */
          case NamedAst.Expression.Var(ident, sym, loc) =>
            // TODO: Lookup the type in the type environment.
            ???

          /*
           * Reference expression.
           */
          case NamedAst.Expression.Ref(id, ref, loc) =>
            ???

          /*
           * Literal expression.
           */
          case NamedAst.Expression.Unit(id, _) => (EmptyContext, Type.Unit)
          case NamedAst.Expression.True(id, loc) => (EmptyContext, Type.Bool)
          case NamedAst.Expression.False(id, loc) => (EmptyContext, Type.Bool)
          case NamedAst.Expression.Char(id, lit, loc) => (EmptyContext, Type.Char)
          case NamedAst.Expression.Float32(id, lit, loc) => (EmptyContext, Type.Float32)
          case NamedAst.Expression.Float64(id, lit, loc) => (EmptyContext, Type.Float64)
          case NamedAst.Expression.Int8(id, lit, loc) => (EmptyContext, Type.Int8)
          case NamedAst.Expression.Int16(id, lit, loc) => (EmptyContext, Type.Int16)
          case NamedAst.Expression.Int32(id, lit, loc) => (EmptyContext, Type.Int32)
          case NamedAst.Expression.Int64(id, lit, loc) => (EmptyContext, Type.Int64)
          case NamedAst.Expression.BigInt(id, lit, loc) => (EmptyContext, Type.BigInt)
          case NamedAst.Expression.Str(id, lit, loc) => (EmptyContext, Type.Str)

          /*
           * Lambda expression.
           */
          case NamedAst.Expression.Lambda(args, body, tpe, loc) => ???

          /*
           * Apply expression.
           */
          case NamedAst.Expression.Apply(id, exp1, args, _) =>
            ???

          /*
           * Unary expression.
           */
          case NamedAst.Expression.Unary(id, op, exp1, _) => op match {
            case UnaryOperator.LogicalNot =>
              val (ctx, tpe) = visitExp(exp1, tenv0)
              constraints += TypeConstraint.Eq((ctx, tpe), (EmptyContext, Type.Bool))
              ret(id, ctx, Type.Bool)

            case UnaryOperator.Plus =>
              val (ctx, tpe) = visitExp(exp1, tenv0)
              ret(id, (TypeClass.Number -> tpe) :: ctx, tpe)

            case UnaryOperator.Minus =>
              val (ctx, tpe) = visitExp(exp1, tenv0)
              ret(id, (TypeClass.Number -> tpe) :: ctx, tpe)

            case UnaryOperator.BitwiseNegate =>
              val (ctx, tpe) = visitExp(exp1, tenv0)
              ret(id, (TypeClass.Number -> tpe) :: ctx, tpe)
          }

          /*
           * Binary expression.
           */
          case NamedAst.Expression.Binary(id, op, e1, e2, _) => op match {
            case BinaryOperator.Plus =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.Number -> tpe1) :: (TypeClass.Number -> tpe2) :: ctx1 ::: ctx2, tpe1)

            case BinaryOperator.Minus =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.Number -> tpe1) :: (TypeClass.Number -> tpe2) :: ctx1 ::: ctx2, tpe1)

            case BinaryOperator.Times =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.Number -> tpe1) :: (TypeClass.Number -> tpe2) :: ctx1 ::: ctx2, tpe1)

            case BinaryOperator.Divide =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.Number -> tpe1) :: (TypeClass.Number -> tpe2) :: ctx1 ::: ctx2, tpe1)

            case BinaryOperator.Modulo =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.Number -> tpe1) :: (TypeClass.Number -> tpe2) :: ctx1 ::: ctx2, tpe1)

            case BinaryOperator.Exponentiate =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.Number -> tpe1) :: (TypeClass.Number -> tpe2) :: ctx1 ::: ctx2, tpe1)

            case BinaryOperator.Equal | BinaryOperator.NotEqual =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.Equal -> tpe1) :: (TypeClass.Equal -> tpe2) :: ctx1 ::: ctx2, Type.Bool)

            case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.TotalOrder -> tpe1) :: (TypeClass.TotalOrder -> tpe2) :: ctx1 ::: ctx2, Type.Bool)

            case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr | BinaryOperator.Implication | BinaryOperator.Biconditional =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (EmptyContext, Type.Bool))
              constraints += TypeConstraint.Eq((ctx2, tpe2), (EmptyContext, Type.Bool))
              ret(id, ctx1 ::: ctx2, Type.Bool)

            case BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor | BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift =>
              val (ctx1, tpe1) = visitExp(e1, tenv0)
              val (ctx2, tpe2) = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
              ret(id, (TypeClass.Number -> tpe1) :: (TypeClass.Number -> tpe2) :: ctx1 ::: ctx2, tpe1)

          }

          /*
           * Let expression.
           */
          case NamedAst.Expression.Let(id, sym, exp1, exp2, loc) =>
            val (ctx1, tpe1) = visitExp(exp1, tenv0)
            val (ctx2, tpe2) = visitExp(exp2, tenv0 + (sym -> tpe1))
            ret(id, ctx1 ::: ctx2, tpe2)

          /*
           * If-then-else expression.
           */
          case NamedAst.Expression.IfThenElse(id, exp1, exp2, exp3, loc) =>
            val (ctx1, tpe1) = visitExp(exp1, tenv0)
            val (ctx2, tpe2) = visitExp(exp2, tenv0)
            val (ctx3, tpe3) = visitExp(exp3, tenv0)
            constraints += TypeConstraint.Eq((ctx1, tpe1), (EmptyContext, Type.Bool))
            constraints += TypeConstraint.Eq((ctx2, tpe2), (ctx3, tpe3))
            ret(id, ctx2 ::: ctx3, tpe2)

          /*
           * Match expression.
           */
          case NamedAst.Expression.Match(exp1, rules, tpe, loc) => ???

          /*
           * Switch expression.
           */
          case NamedAst.Expression.Switch(id, rules, loc) =>
            ???
          //            val bodyTypes = mutable.ListBuffer.empty[Type]
          //            for ((cond, body) <- rules) {
          //              val condType = visitExp(cond, tenv0)
          //              bodyTypes += visitExp(body, tenv0)
          //              constraints += TypeConstraint.Eq(condType, Type.Bool)
          //            }
          //
          //            constraints += TypeConstraint.AllEq(bodyTypes.toList)
          //
          //            bodyTypes.head // TODO: Or generate fresh symbol?

          /*
           * Tag expression.
           */
          case NamedAst.Expression.Tag(id, enum, tag, exp, loc) =>
            // TODO
            val tpe = visitExp(exp, tenv0)
            ???

          /*
           * Tuple expression.
           */
          case NamedAst.Expression.Tuple(id, elms, loc) =>
            val elements = elms.map(e => visitExp(e, tenv0))
            val contexts = elements.flatMap(_._1)
            val types = elements.map(_._2)
            ret(id, contexts, Type.Tuple(types))

          /*
           * None expression.
           */
          case NamedAst.Expression.FNone(id, loc) =>
            ret(id, EmptyContext, Type.FOpt)

          /*
           * Some expression.
           */
          case NamedAst.Expression.FSome(id, exp, loc) =>
            val (ctx, tpe) = visitExp(exp, tenv0)
            ret(id, ctx, Type.FOpt)

          /*
           * Nil expression.
           */
          case NamedAst.Expression.FNil(id, loc) =>
            ???
          //ret(id, EmptyContext, Type.FList(fresh()))

          /*
           * List expression.
           */
          case NamedAst.Expression.FList(id, hd, tl, loc) =>
            val (ctx1, tpe1) = visitExp(hd, tenv0)
            val (ctx2, tpe2) = visitExp(tl, tenv0)
            constraints += TypeConstraint.Eq((ctx1, tpe1), (ctx2, tpe2))
            ???
          //ret(id, ctx1 ::: ctx2, Type.FList(tpe1))

          /*
           * Vector expression.
           */
          case NamedAst.Expression.FVec(id, Nil, loc) =>
            //ret(id, EmptyContext, Type.FVec(fresh()))
            ???

          case NamedAst.Expression.FVec(id, elms, loc) =>
            val elements = elms.map(e => visitExp(e, tenv0))
            for ((ctx, tpe) <- elements.tail) {
              // assert that each element has the same type as the first element.
              val (ctx0, tpe0) = elements.head
              constraints += TypeConstraint.Eq((ctx0, tpe0), (ctx, tpe))
            }
            val contexts = elements.flatMap(_._1)
            ret(id, contexts, elements.head._2)

          /*
           * Set expression.
           */
          case NamedAst.Expression.FSet(id, elms, loc) =>
            ??? // TODO

          /*
           * Map expression.
           */
          case NamedAst.Expression.FMap(id, elms, loc) =>
            ??? // TODO

          /*
           * GetIndex expression.
           */
          case NamedAst.Expression.GetIndex(id, exp1, exp2, loc) =>
            val (ctx1, tpe1) = visitExp(exp1, tenv0)
            val (ctx2, tpe2) = visitExp(exp2, tenv0)
            constraints += TypeConstraint.Eq((ctx2, tpe2), (EmptyContext, Type.Int32))
            ret(id, (TypeClass.Indexed -> tpe1) :: ctx1 ::: ctx2, tpe1)

          /*
           * PutIndex expression.
           */
          case NamedAst.Expression.PutIndex(id, exp1, exp2, exp3, loc) =>
            val (ctx1, tpe1) = visitExp(exp1, tenv0)
            val (ctx2, tpe2) = visitExp(exp2, tenv0)
            val (ctx3, tpe3) = visitExp(exp3, tenv0)
            constraints += TypeConstraint.Eq((ctx2, tpe2), (EmptyContext, Type.Int32))
            ret(id, (TypeClass.Indexed -> tpe1) :: (TypeClass.Indexed -> tpe3) :: ctx1 ::: ctx2 ::: ctx3, tpe1)

          /*
           * Existential expression.
           */
          case NamedAst.Expression.Existential(id, params, e, loc) =>
            val tenv = params.foldLeft(tenv0) {
              case (macc, NamedAst.FormalParam(sym, t)) => macc + (sym -> t)
            }
            val (ctx, tpe) = visitExp(e, tenv)
            constraints += TypeConstraint.Eq((ctx, tpe), (EmptyContext, Type.Bool))
            ret(id, ctx, Type.Bool)

          /*
           * Universal expression.
           */
          case NamedAst.Expression.Universal(id, params, e, loc) =>
            val tenv = params.foldLeft(tenv0) {
              case (macc, NamedAst.FormalParam(sym, t)) => macc + (sym -> t)
            }
            val (ctx, tpe) = visitExp(e, tenv)
            constraints += TypeConstraint.Eq((ctx, tpe), (EmptyContext, Type.Bool))
            ret(id, ctx, Type.Bool)

          /*
           * Ascribe expression.
           */
          case NamedAst.Expression.Ascribe(id, exp, tpe, loc) =>
            val (ctx, tpe) = visitExp(exp, tenv0)
            constraints += TypeConstraint.Eq((ctx, tpe), (EmptyContext, tpe))
            ret(id, ctx, tpe)

          /*
           * User Error expression.
           */
          case NamedAst.Expression.UserError(id, loc) =>
            ret(id, EmptyContext, fresh())

        }

        /**
          * Generates type constraints for the given pattern `p0` under the given type environment `tenv`.
          */
        def visitPat(p0: NamedAst.Pattern, tenv: Map[Name.Ident, Type]): Type = p0 match {
          case NamedAst.Pattern.Wild(loc) => ???
          case NamedAst.Pattern.Var(ident, loc) => ???
          case NamedAst.Pattern.Unit(loc) => Type.Unit
          case NamedAst.Pattern.True(_) => Type.Bool
          case NamedAst.Pattern.False(_) => Type.Bool
          case NamedAst.Pattern.Char(_, _) => Type.Char
          case NamedAst.Pattern.Float32(_, _) => Type.Float32
          case NamedAst.Pattern.Float64(_, _) => Type.Float64
          case NamedAst.Pattern.Int8(_, _) => Type.Int8
          case NamedAst.Pattern.Int16(_, _) => Type.Int16
          case NamedAst.Pattern.Int32(_, _) => Type.Int32
          case NamedAst.Pattern.Int64(_, _) => Type.Int64
          case NamedAst.Pattern.BigInt(_, _) => Type.BigInt
          case NamedAst.Pattern.Str(_, _) => Type.Str
          case NamedAst.Pattern.Tag(enum, tag, p1, loc) => ???
          case NamedAst.Pattern.Tuple(elms, loc) =>
            val tpes = elms.map(e => visitPat(e, tenv))
            Type.Tuple(tpes)

          case _ => ???
        }

        visitExp(exp0, Map.empty)

      }
    }

  }

  /**
    * Returns the most general unifier of the two given types `tpe1` and `tpe2`.
    *
    * Returns [[Failure]] if the two types cannot be unified.
    */
  def unify(tpe1: Type, tpe2: Type): Validation[Substitution, TypeError] = (tpe1, tpe2) match {
    case (x: Type.Var, _) => unifyVar(x, tpe2)
    case (_, x: Type.Var) => unifyVar(x, tpe1)
    case (Type.Unit, Type.Unit) => Substitution.empty.toSuccess
    case (Type.Bool, Type.Bool) => Substitution.empty.toSuccess
    case (Type.Char, Type.Char) => Substitution.empty.toSuccess
    case (Type.Float32, Type.Float32) => Substitution.empty.toSuccess
    case (Type.Float64, Type.Float64) => Substitution.empty.toSuccess
    case (Type.Int8, Type.Int8) => Substitution.empty.toSuccess
    case (Type.Int16, Type.Int16) => Substitution.empty.toSuccess
    case (Type.Int32, Type.Int32) => Substitution.empty.toSuccess
    case (Type.Int64, Type.Int64) => Substitution.empty.toSuccess
    case (Type.BigInt, Type.BigInt) => Substitution.empty.toSuccess
    case (Type.Str, Type.Str) => Substitution.empty.toSuccess
    case (Type.Native, Type.Native) => Substitution.empty.toSuccess
    case (Type.Arrow, Type.Arrow) => Substitution.empty.toSuccess
    case (Type.FTuple(l1), Type.FTuple(l2)) if l1 == l2 => Substitution.empty.toSuccess
    case (Type.FOpt, Type.FOpt) => Substitution.empty.toSuccess
    case (Type.FList, Type.FList) => Substitution.empty.toSuccess
    case (Type.FVec, Type.FVec) => Substitution.empty.toSuccess
    case (Type.FSet, Type.FSet) => Substitution.empty.toSuccess
    case (Type.FMap, Type.FMap) => Substitution.empty.toSuccess
    case (Type.Enum(name1, _), Type.Enum(name2, _)) if name1 == name2 => ??? // TODO: Need to unify inside cases?
    case (Type.Apply(t11, t12), Type.Apply(t21, t22)) =>
      unify(t11, t21) flatMap {
        case subst1 => unify(subst1(t12), subst1(t22)) map {
          case subst2 => subst2 @@ subst1
        }
      }
    case _ => TypeError.UnificationError(tpe1, tpe2).toFailure
  }

  /**
    * Unifies the given variable `x` with the given type `tpe`.
    *
    * Performs the so-called occurs-check to ensure that the substitution is kind-preserving.
    */
  def unifyVar(x: Type.Var, tpe: Type): Validation[Substitution, TypeError] = {
    if (x == tpe) {
      return Substitution.empty.toSuccess
    }
    if (tpe.typeVars contains x) {
      return TypeError.OccursCheck().toFailure
    }
    if (x.kind != tpe.kind) {
      return TypeError.KindError().toFailure
    }
    return Substitution.singleton(x, tpe).toSuccess
  }

  /**
    * Reassembles the given expression `exp0` under the given type environment `tenv0`.
    */
  def reassemble(exp0: NamedAst.Expression, tenv0: Map[Int, Type]): TypedAst.Expression = exp0 match {
    /*
     * Wildcard expression.
     */
    case NamedAst.Expression.Wild(id, loc) => ??? // TODO

    /*
     * Variable expression.
     */
    case NamedAst.Expression.Var(id, sym, loc) => TypedAst.Expression.Var(???, tenv0(id), loc) // TODO

    /*
     * Reference expression.
     */
    case NamedAst.Expression.Ref(id, ref, loc) => TypedAst.Expression.Ref(???, tenv0(id), loc) // TODO

    /*
     * Literal expression.
     */
    case NamedAst.Expression.Unit(id, loc) => TypedAst.Expression.Unit(loc)
    case NamedAst.Expression.True(id, loc) => TypedAst.Expression.True(loc)
    case NamedAst.Expression.False(id, loc) => TypedAst.Expression.False(loc)
    case NamedAst.Expression.Char(id, lit, loc) => TypedAst.Expression.Char(lit, loc)
    case NamedAst.Expression.Float32(id, lit, loc) => TypedAst.Expression.Float32(lit, loc)
    case NamedAst.Expression.Float64(id, lit, loc) => TypedAst.Expression.Float64(lit, loc)
    case NamedAst.Expression.Int8(id, lit, loc) => TypedAst.Expression.Int8(lit, loc)
    case NamedAst.Expression.Int16(id, lit, loc) => TypedAst.Expression.Int16(lit, loc)
    case NamedAst.Expression.Int32(id, lit, loc) => TypedAst.Expression.Int32(lit, loc)
    case NamedAst.Expression.Int64(id, lit, loc) => TypedAst.Expression.Int64(lit, loc)
    case NamedAst.Expression.BigInt(id, lit, loc) => TypedAst.Expression.BigInt(lit, loc)
    case NamedAst.Expression.Str(id, lit, loc) => TypedAst.Expression.Str(lit, loc)

    /*
     * Apply expression.
     */
    case NamedAst.Expression.Apply(id, lambda, args, loc) => ??? // TODO

    /*
     * Lambda expression.
     */
    case NamedAst.Expression.Lambda(id, params, exp, loc) => ??? // TODO

    /*
     * Unary expression.
     */
    case NamedAst.Expression.Unary(id, op, exp, loc) =>
      val e = reassemble(exp, tenv0)
      TypedAst.Expression.Unary(op, e, tenv0(id), loc)

    /*
     * Binary expression.
     */
    case NamedAst.Expression.Binary(id, op, exp1, exp2, loc) =>
      val e1 = reassemble(exp1, tenv0)
      val e2 = reassemble(exp2, tenv0)
      TypedAst.Expression.Binary(op, e1, e2, tenv0(id), loc)

    /*
     * If-then-else expression.
     */
    case NamedAst.Expression.IfThenElse(id, exp1, exp2, exp3, loc) =>
      val e1 = reassemble(exp1, tenv0)
      val e2 = reassemble(exp2, tenv0)
      val e3 = reassemble(exp3, tenv0)
      TypedAst.Expression.IfThenElse(e1, e2, e3, tenv0(id), loc)

    /*
     * Let expression.
     */
    case NamedAst.Expression.Let(id, sym, exp1, exp2, loc) => ??? // TODO

    /*
     * Match expression.
     */
    case NamedAst.Expression.Match(id, exp, rules, loc) => ??? // TODO

    /*
     * Switch expression.
     */
    case NamedAst.Expression.Switch(id, rules, loc) =>
      val rs = rules.map {
        case (cond, body) => (reassemble(cond, tenv0), reassemble(body, tenv0))
      }
      TypedAst.Expression.Switch(rs, tenv0(id), loc)

    /*
     * Tag expression.
     */
    case NamedAst.Expression.Tag(id, enum, tag, exp, loc) =>
      val e = reassemble(exp, tenv0)
      val tpe: Type.Enum = tenv0(id).asInstanceOf[Type.Enum]
      TypedAst.Expression.Tag(???, tag, e, tpe, loc) // TODO

    /*
     * Tuple expression.
     */
    case NamedAst.Expression.Tuple(id, elms, loc) =>
      val es = elms.map(e => reassemble(e, tenv0))
      val tpe: Type.Tuple = tenv0(id).asInstanceOf[Type.Tuple]
      TypedAst.Expression.Tuple(es, tpe, loc)

    /*
     * None expression.
     */
    case NamedAst.Expression.FNone(id, loc) =>
      // TODO
      ???
    //val tpe: Type.FOpt = tenv0(id).asInstanceOf[Type.FOpt]
    //TypedAst.Expression.FNone(tpe, loc)

    /*
     * Some expression.
     */
    case NamedAst.Expression.FSome(id, exp, loc) =>
      ???
    //val tpe: Type.FOpt = tenv0(id).asInstanceOf[Type.FOpt]
    //TypedAst.Expression.FSome(reassemble(exp, tenv0), tpe, loc)

    /*
     * Nil expression.
     */
    case NamedAst.Expression.FNil(id, loc) =>
      ???
    //val tpe: Type.FList = tenv0(id).asInstanceOf[Type.FList]
    //TypedAst.Expression.FNil(tpe, loc)

    /*
     * List expression.
     */
    case NamedAst.Expression.FList(id, hd, tl, loc) =>
      val e1 = reassemble(hd, tenv0)
      val e2 = reassemble(tl, tenv0)
      //val tpe: Type.FList = tenv0(id).asInstanceOf[Type.FList]
      //TypedAst.Expression.FList(e1, e2, tpe, loc)
      ???

    /*
     * Vec expression.
     */
    case NamedAst.Expression.FVec(id, elms, loc) =>
      val es = elms.map(e => reassemble(e, tenv0))
      //val tpe: Type.FVec = tenv0(id).asInstanceOf[Type.FVec]
      //TypedAst.Expression.FVec(es, tpe, loc)
      ???

    /*
     * Set expression.
     */
    case NamedAst.Expression.FSet(id, elms, loc) =>
      val es = elms.map(e => reassemble(e, tenv0))
      //val tpe: Type.FSet = tenv0(id).asInstanceOf[Type.FSet]
      //TypedAst.Expression.FSet(es, tpe, loc)
      ???

    /*
     * Map expression.
     */
    case NamedAst.Expression.FMap(id, elms, loc) =>
      val es = elms map {
        case (key, value) => (reassemble(key, tenv0), reassemble(value, tenv0))
      }
      //val tpe: Type.FMap = tenv0(id).asInstanceOf[Type.FMap]
      //TypedAst.Expression.FMap(es, tpe, loc)
      ???

    /*
     * GetIndex expression.
     */
    case NamedAst.Expression.GetIndex(id, exp1, exp2, loc) =>
      val e1 = reassemble(exp1, tenv0)
      val e2 = reassemble(exp2, tenv0)
      TypedAst.Expression.GetIndex(e1, e2, tenv0(id), loc)

    /*
     * PutIndex expression.
     */
    case NamedAst.Expression.PutIndex(id, exp1, exp2, exp3, loc) =>
      val e1 = reassemble(exp1, tenv0)
      val e2 = reassemble(exp2, tenv0)
      val e3 = reassemble(exp3, tenv0)
      TypedAst.Expression.PutIndex(e1, e2, e3, tenv0(id), loc)

    /*
     * Existential expression.
     */
    case NamedAst.Expression.Existential(id, params, exp, loc) =>
      val e = reassemble(exp, tenv0)
      //TypedAst.Expression.Existential(params, e, loc)
      ???

    /*
     * Universal expression.
     */
    case NamedAst.Expression.Universal(id, params, exp, loc) =>
      val e = reassemble(exp, tenv0)
      //TypedAst.Expression.Universal(params, e, loc)
      ???

    /*
     * Ascribe expression.
     */
    case NamedAst.Expression.Ascribe(id, exp, tpe, loc) =>
      // simply reassemble the nested expression.
      reassemble(exp, tenv0)

    /*
     * User Error expression.
     */
    case NamedAst.Expression.UserError(id, loc) =>
      TypedAst.Expression.Error(tenv0(id), loc)
  }

  /**
    * Returns a fresh type variable.
    */
  @inline
  private def fresh()(implicit genSym: GenSym): Type.Var = genSym.freshTypeVar()

}
