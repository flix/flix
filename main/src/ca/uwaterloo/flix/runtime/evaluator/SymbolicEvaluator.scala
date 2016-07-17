/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.evaluator

import ca.uwaterloo.flix.api.{MatchException, SwitchException, UserException}
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.GenSym
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * Symbolic evaluator that supports symbolic values and collects path constraints.
  */
object SymbolicEvaluator {

  /**
    * The type of path constraints.
    *
    * A path constraint is a disjunction of SMT expressions.
    * Path constraints are collected during execution of the program.
    */
  type PathConstraint = List[SmtExpr]

  /**
    * The type of environments.
    *
    * An environment is map from variables names to symbolic values.
    */
  type Environment = Map[String, SymVal]

  /**
    * The type of contexts.
    *
    * A context is a list of (path constraint, symbolic value) pairs.
    * Each pair corresponds to one execution path through the program.
    */
  type Context = List[(PathConstraint, SymVal)]

  /**
    * Evaluates the given expression `exp0` under the given environment `env0`.
    */
  def eval(exp0: Expression, env0: Map[String, SymVal], root: ExecutableAst.Root)(implicit genSym: GenSym): Context = {
    /*
      * Local visitor.
      */
    def eval(pc0: PathConstraint, exp0: Expression, env0: Environment)(implicit genSym: GenSym): Context = exp0 match {
      /**
        * Unit.
        */
      case Expression.Unit => lift(pc0, SymVal.Unit)

      /**
        * True.
        */
      case Expression.True => lift(pc0, SymVal.True)

      /**
        * False.
        */
      case Expression.False => lift(pc0, SymVal.False)

      /**
        * Char.
        */
      case Expression.Char(lit) => lift(pc0, SymVal.Char(lit))

      /**
        * Float32.
        */
      case Expression.Float32(lit) => lift(pc0, SymVal.Float32(lit))

      /**
        * Float64.
        */
      case Expression.Float64(lit) => lift(pc0, SymVal.Float64(lit))

      /**
        * Int8.
        */
      case Expression.Int8(lit) => lift(pc0, SymVal.Int8(lit))

      /**
        * Int16.
        */
      case Expression.Int16(lit) => lift(pc0, SymVal.Int16(lit))

      /**
        * Int32.
        */
      case Expression.Int32(lit) => lift(pc0, SymVal.Int32(lit))

      /**
        * Int64.
        */
      case Expression.Int64(lit) => lift(pc0, SymVal.Int64(lit))

      /**
        * BigInt.
        */
      case Expression.BigInt(lit) => lift(pc0, SymVal.BigInt(lit))

      /**
        * Str.
        */
      case Expression.Str(lit) => lift(pc0, SymVal.Str(lit))

      /**
        * Local Variable.
        */
      case Expression.Var(ident, _, tpe, loc) => lift(pc0, env0(ident.name))

      /**
        * Reference.
        */
      case Expression.Ref(name, tpe, loc) =>
        // Lookup and evaluate the definition.
        root.constants.get(name) match {
          case None => throw InternalCompilerException(s"Type Error: Unresolved reference '$name'.")
          case Some(defn) => eval(pc0, defn.exp, env0)
        }

      /**
        * Closure.
        */
      case Expression.MkClosureRef(ref, freeVars, _, _) =>
        // Save the values of the free variables in a list.
        // When the closure is called, these values will be provided at the beginning of the argument list.
        val env = freeVars.toList.map(f => env0(f.ident.name))
        // Construct the closure.
        val clo = SymVal.Closure(ref, env)
        lift(pc0, clo)

      /**
        * Apply Reference.
        */
      case Expression.ApplyRef(name, args, _, _) =>
        // Lookup the reference.
        val defn = root.constants(name)
        // Evaluate all the arguments.
        evaln(pc0, args, env0) flatMap {
          case (pc, as) =>
            // Bind the actual arguments to the formal variables.
            val newEnv = (defn.formals zip as).foldLeft(Map.empty[String, SymVal]) {
              case (macc, (formal, actual)) => macc + (formal.ident.name -> actual)
            }
            // Evaluate the body under the new environment.
            eval(pc, defn.exp, newEnv)
        }

      /**
        * Apply Closure.
        */
      // TODO: check this implementation.
      case Expression.ApplyClosure(exp, args, _, _) =>
        // Evaluate the closure.
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Closure(ref, bindings)) =>
            // Lookup the definition
            val defn = root.constants(ref.name)
            // Evaluate all the arguments.
            evaln(pc, args, env0) flatMap {
              case (pc1, actuals) =>
                // Construct the environment
                val newArgs = bindings ++ actuals
                val newEnv = (defn.formals zip newArgs).foldLeft(Map.empty[String, SymVal]) {
                  case (macc, (formal, actual)) => macc + (formal.ident.name -> actual)
                }
                eval(pc1, defn.exp, newEnv)
            }
          case (_, v) => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * Unary.
        */
      case Expression.Unary(op, exp, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, v) => op match {
            /**
              * Unary Not.
              */
            case UnaryOperator.LogicalNot => v match {
              // Concrete semantics.
              case SymVal.True => lift(pc, SymVal.False)
              case SymVal.False => lift(pc, SymVal.True)

              // Symbolic Semantics.
              case SymVal.AtomicVar(id) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.False),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.True)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
            }

            /**
              * Unary Plus.
              */
            case UnaryOperator.Plus => lift(pc, v)

            /**
              * Unary Minus.
              */
            case UnaryOperator.Minus => v match {
              // Concrete semantics.
              case SymVal.Int8(i) => lift(pc, SymVal.Int8((-i).toByte))
              case SymVal.Int16(i) => lift(pc, SymVal.Int16((-i).toShort))
              case SymVal.Int32(i) => lift(pc, SymVal.Int32(-i))
              case SymVal.Int64(i) => lift(pc, SymVal.Int64(-i))
              case SymVal.BigInt(i) => lift(pc, SymVal.BigInt(i.negate))

              // Symbolic semantics.
              case SymVal.AtomicVar(id) =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Minus(zeroOf(exp.tpe), SmtExpr.Var(id, exp.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
            }

            /**
              * Unary Bitwise Negate.
              */
            case UnaryOperator.BitwiseNegate => v match {
              // Concrete semantics
              case SymVal.Int8(i) => lift(pc, SymVal.Int8((~i).toByte))
              case SymVal.Int16(i) => lift(pc, SymVal.Int16((~i).toShort))
              case SymVal.Int32(i) => lift(pc, SymVal.Int32(~i))
              case SymVal.Int64(i) => lift(pc, SymVal.Int64(~i))
              case SymVal.BigInt(i) => lift(pc, SymVal.BigInt(i.not))

              // Symbolic semantics
              case SymVal.AtomicVar(id) =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseNegate(SmtExpr.Var(id, exp.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
            }

          }
        }

      /**
        * Binary.
        */
      case Expression.Binary(op, exp1, exp2, _, _) =>
        eval2(pc0, exp1, exp2, env0) flatMap {
          case (pc, (v1, v2)) => op match {

            /**
              * Plus.
              */
            case BinaryOperator.Plus => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 + i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 + i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 + i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 + i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, SymVal.BigInt(i1 add i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Plus(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Minus.
              */
            case BinaryOperator.Minus => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 - i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 - i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 - i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 - i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, SymVal.BigInt(i1 subtract i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Minus(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Times.
              */
            case BinaryOperator.Times => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 * i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 * i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 * i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 * i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, SymVal.BigInt(i1 multiply i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Times(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Divide.
              */
            case BinaryOperator.Divide => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 / i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 / i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 / i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 / i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, SymVal.BigInt(i1 divide i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Divide(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Modulo.
              */
            case BinaryOperator.Modulo => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 % i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 % i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 % i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 % i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, SymVal.BigInt(i1 remainder i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Modulo(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Exponentiate.
              */
            case BinaryOperator.Exponentiate => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8(Math.pow(i1, i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16(Math.pow(i1, i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(Math.pow(i1, i2).toInt))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(Math.pow(i1, i2).toLong))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => throw InternalCompilerException(s"Type Error: BigInt does not support Exponentiate.")

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Exponentiate(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Less.
              */
            case BinaryOperator.Less => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, toBool(i1 < i2))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, toBool(i1 < i2))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, toBool(i1 < i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, toBool(i1 < i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, toBool(i1.compareTo(i2) < 0))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, Type.Bool), SmtExpr.Less(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * LessEqual.
              */
            case BinaryOperator.LessEqual => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, toBool(i1 <= i2))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, toBool(i1 <= i2))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, toBool(i1 <= i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, toBool(i1 <= i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, toBool(i1.compareTo(i2) <= 0))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, Type.Bool), SmtExpr.LessEqual(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Greater.
              */
            case BinaryOperator.Greater => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, toBool(i1 > i2))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, toBool(i1 > i2))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, toBool(i1 > i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, toBool(i1 > i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, toBool(i1.compareTo(i2) > 0))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, Type.Bool), SmtExpr.Greater(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * GreaterEqual.
              */
            case BinaryOperator.GreaterEqual => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, toBool(i1 >= i2))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, toBool(i1 >= i2))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, toBool(i1 >= i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, toBool(i1 >= i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, toBool(i1.compareTo(i2) >= 0))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, Type.Bool), SmtExpr.GreaterEqual(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Equal.
              */
            case BinaryOperator.Equal => eq(pc, v1, v2, exp1.tpe)

            /**
              * Not Equal.
              */
            case BinaryOperator.NotEqual => eq(pc, v1, v2, exp1.tpe).flatMap {
              case (pc1, SymVal.True) => lift(pc1, SymVal.False)
              case (pc1, SymVal.False) => lift(pc1, SymVal.True)
              case (_, v) => throw InternalCompilerException(s"Type Error: Unexpected value:'$v'.")
            }

            /**
              * Logical And.
              */
            case BinaryOperator.LogicalAnd => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.False)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id)) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.False)
              )
              case (SymVal.AtomicVar(id), SymVal.True) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.False)
              )

              case (SymVal.False, SymVal.AtomicVar(id)) => lift(pc, SymVal.False)
              case (SymVal.AtomicVar(id), SymVal.False) => lift(pc, SymVal.False)

              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) => List(
                (SmtExpr.LogicalAnd(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool)) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.LogicalAnd(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool))) :: pc, SymVal.False)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 && $v2'.")
            }

            /**
              * Logical Or.
              */
            case BinaryOperator.LogicalOr => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.False)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id)) => lift(pc, SymVal.True)
              case (SymVal.AtomicVar(id), SymVal.True) => lift(pc, SymVal.True)

              case (SymVal.False, SymVal.AtomicVar(id)) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.False)
              )
              case (SymVal.AtomicVar(id), SymVal.False) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.False)
              )

              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) => List(
                (SmtExpr.LogicalOr(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool)) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.LogicalOr(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool))) :: pc, SymVal.False)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 || $v2'.")
            }

            /**
              * Logical Implication.
              */
            case BinaryOperator.Implication => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.True)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id)) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.False)
              )
              case (SymVal.False, SymVal.AtomicVar(id)) => lift(pc, SymVal.True)

              case (SymVal.AtomicVar(id), SymVal.False) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.False),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.True)
              )
              case (SymVal.AtomicVar(id), SymVal.True) => lift(pc, SymVal.True)

              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) => List(
                (SmtExpr.Implication(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool)) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Implication(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool))) :: pc, SymVal.True)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 => $v2'.")
            }

            /**
              * Logical Bicondition.
              */
            case BinaryOperator.Biconditional => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.True) => lift(pc, SymVal.False)
              case (SymVal.False, SymVal.False) => lift(pc, SymVal.True)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id)) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.False)
              )
              case (SymVal.AtomicVar(id), SymVal.True) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.False)
              )

              case (SymVal.False, SymVal.AtomicVar(id)) => List(
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.True),
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.False)
              )
              case (SymVal.AtomicVar(id), SymVal.False) => List(
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, SymVal.True),
                (SmtExpr.Var(id, Type.Bool) :: pc, SymVal.False)
              )

              case (SymVal.AtomicVar(id1), SymVal.AtomicVar(id2)) => List(
                (SmtExpr.Bicondition(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool)) :: pc, SymVal.True),
                (SmtExpr.Not(SmtExpr.Bicondition(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool))) :: pc, SymVal.False)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 <==> $v2'.")
            }

            /**
              * Bitwise And.
              */
            case BinaryOperator.BitwiseAnd => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 & i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 & i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 & i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 & i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, SymVal.BigInt(i1 and i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseAnd(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Bitwise Or.
              */
            case BinaryOperator.BitwiseOr => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 | i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 | i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 | i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 | i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, SymVal.BigInt(i1 or i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseOr(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Bitwise Xor.
              */
            case BinaryOperator.BitwiseXor => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, SymVal.Int8((i1 ^ i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, SymVal.Int16((i1 ^ i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 ^ i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, SymVal.Int64(i1 ^ i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, SymVal.BigInt(i1 xor i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseXor(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Bitwise Left Shift.
              */
            case BinaryOperator.BitwiseLeftShift => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int8((i1 << i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int16((i1 << i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 << i2))
              case (SymVal.Int64(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int64(i1 << i2))
              case (SymVal.BigInt(i1), SymVal.Int32(i2)) => lift(pc, SymVal.BigInt(i1 shiftLeft i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseLeftShift(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }

            /**
              * Bitwise Right Shift.
              */
            case BinaryOperator.BitwiseRightShift => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int8((i1 >> i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int16((i1 >> i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int32(i1 >> i2))
              case (SymVal.Int64(i1), SymVal.Int32(i2)) => lift(pc, SymVal.Int64(i1 >> i2))
              case (SymVal.BigInt(i1), SymVal.Int32(i2)) => lift(pc, SymVal.BigInt(i1 shiftRight i2))

              // Symbolic semantics.
              case _ =>
                val newVar = genSym.fresh2()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseRightShift(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, SymVal.AtomicVar(newVar))
            }
          }
        }

      /**
        * If-then-else.
        */
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        eval(pc0, exp1, env0) flatMap {
          case (pc, c) => c match {
            case SymVal.True => eval(pc, exp2, env0)
            case SymVal.False => eval(pc, exp3, env0)
            case SymVal.AtomicVar(id) =>
              // Evaluate both branches under different path constraints.
              val consequent = eval(SmtExpr.Var(id, Type.Bool) :: pc, exp2, env0)
              val alternative = eval(SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, exp3, env0)
              consequent ++ alternative
            case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
          }
        }

      /**
        * Let-binding.
        */
      case Expression.Let(ident, _, exp1, exp2, _, _) =>
        eval(pc0, exp1, env0) flatMap {
          case (pc, v1) =>
            // Bind the variable to the value of `exp1` which is `v1`.
            val newEnv = env0 + (ident.name -> v1)
            eval(pc, exp2, newEnv)
        }

      /**
        * Tags.
        */
      case Expression.Tag(enum, tag, exp, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, v) => lift(pc, SymVal.Tag(tag.name, v))
        }

      /**
        * Tuples.
        */
      case Expression.Tuple(elms, _, _) =>
        evaln(pc0, elms, env0) flatMap {
          case (pc, es) => lift(pc, SymVal.Tuple(es))
        }

      /**
        * Check Tag Value.
        */
      case Expression.CheckTag(tag, exp, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Tag(tag2, _)) =>
            if (tag.name == tag2)
              lift(pc, SymVal.True)
            else
              lift(pc, SymVal.False)
          case (_, v) => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * Get Tag Value.
        */
      case Expression.GetTagValue(tag, exp, _, _) =>
        eval(pc0, exp, env0) flatMap {
          case (pc, SymVal.Tag(_, v)) => lift(pc, v)
          case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * Get Tuple Index.
        */
      case Expression.GetTupleIndex(base, offset, _, _) =>
        eval(pc0, base, env0) flatMap {
          case (pc, SymVal.Tuple(elms)) => lift(pc, elms(offset))
          case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * User Error.
        */
      case Expression.UserError(tpe, loc) => throw new UserException("User Error.", loc)

      /**
        * Match Error.
        */
      case Expression.MatchError(tpe, loc) => throw new MatchException("Match Error.", loc)

      /**
        * Switch Error
        */
      case Expression.SwitchError(tpe, loc) => throw new SwitchException("Switch Error", loc)

      // NB: Not yet fully implemented in the backend.
      case e: Expression.FSet => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.CheckNil => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.CheckCons => throw InternalCompilerException(s"Unsupported expression: '$e'.")

      /**
        * Unsupported expressions.
        */
      case e: Expression.ApplyHook => throw new InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.Universal => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.Existential => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.LoadBool => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.LoadInt8 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.LoadInt16 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.LoadInt32 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.StoreBool => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.StoreInt8 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.StoreInt16 => throw InternalCompilerException(s"Unsupported expression: '$e'.")
      case e: Expression.StoreInt32 => throw InternalCompilerException(s"Unsupported expression: '$e'.")

    }

    /**
      * Returns a context with the value `v` guarded by the path constraint `pc`.
      */
    def lift(pc0: PathConstraint, v: SymVal): Context = List(pc0 -> v)

    /**
      * Test equality of `x` and `y` (of type `tpe`) under the path constraint `pc0`.
      */
    def eq(pc0: PathConstraint, x: SymVal, y: SymVal, tpe: Type): Context = (x, y) match {
      /**
        * Variable.
        */
      case (SymVal.AtomicVar(ident1), SymVal.AtomicVar(ident2)) =>
        // Equality of two atomic variables is encoded using two path constraints.
        List(
          (SmtExpr.Equal(SmtExpr.Var(ident1, tpe), SmtExpr.Var(ident2, tpe)) :: pc0, SymVal.True),
          (SmtExpr.NotEqual(SmtExpr.Var(ident1, tpe), SmtExpr.Var(ident2, tpe)) :: pc0, SymVal.False)
        )

      /**
        * Unit.
        */
      case (SymVal.Unit, SymVal.Unit) => lift(pc0, SymVal.True)

      /**
        * True & False.
        */
      case (SymVal.True, SymVal.True) => lift(pc0, SymVal.True)
      case (SymVal.True, SymVal.False) => lift(pc0, SymVal.False)
      case (SymVal.False, SymVal.True) => lift(pc0, SymVal.False)
      case (SymVal.False, SymVal.False) => lift(pc0, SymVal.True)

      /**
        * Char.
        */
      case (SymVal.Char(c1), SymVal.Char(c2)) => lift(pc0, toBool(c1 == c2))

      /**
        * Float32
        */
      case (SymVal.Float32(f1), SymVal.Float64(f2)) => lift(pc0, toBool(f1 == f2))

      /**
        * Float64
        */
      case (SymVal.Float64(f1), SymVal.Float64(f2)) => lift(pc0, toBool(f1 == f2))

      /**
        * Int8.
        */
      // Concrete semantics.
      case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc0, toBool(i1 == i2))
      // Symbolic semantics.  
      case (SymVal.AtomicVar(id), SymVal.Int8(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int8), SmtExpr.Int8(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.Int8), SmtExpr.Int8(i2)) :: pc0, SymVal.False)
      )
      case (SymVal.Int8(i2), SymVal.AtomicVar(id)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int8), SmtExpr.Int8(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Int8(i2), SmtExpr.Var(id, Type.Int8)) :: pc0, SymVal.False)
      )

      /**
        * Int16.
        */
      // Concrete semantics.
      case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc0, toBool(i1 == i2))
      // Symbolic semantics.
      case (SymVal.AtomicVar(id), SymVal.Int16(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int16), SmtExpr.Int16(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.Int16), SmtExpr.Int16(i2)) :: pc0, SymVal.False)
      )
      case (SymVal.Int16(i2), SymVal.AtomicVar(id)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int16), SmtExpr.Int16(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Int16(i2), SmtExpr.Var(id, Type.Int16)) :: pc0, SymVal.False)
      )

      /**
        * Int32.
        */
      // Concrete semantics.
      case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc0, toBool(i1 == i2))
      // Symbolic semantics.
      case (SymVal.AtomicVar(id), SymVal.Int32(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int32), SmtExpr.Int32(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.Int32), SmtExpr.Int32(i2)) :: pc0, SymVal.False)
      )
      case (SymVal.Int32(i2), SymVal.AtomicVar(id)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int32), SmtExpr.Int32(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Int32(i2), SmtExpr.Var(id, Type.Int32)) :: pc0, SymVal.False)
      )

      /**
        * Int64.
        */
      // Concrete semantics.
      case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc0, toBool(i1 == i2))
      // Symbolic semantics.
      case (SymVal.AtomicVar(id), SymVal.Int64(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int64), SmtExpr.Int64(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.Int64), SmtExpr.Int64(i2)) :: pc0, SymVal.False)
      )
      case (SymVal.Int64(i2), SymVal.AtomicVar(id)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int64), SmtExpr.Int64(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Int64(i2), SmtExpr.Var(id, Type.Int64)) :: pc0, SymVal.False)
      )

      /**
        * BigInt.
        */
      // Concrete semantics.
      case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc0, toBool(i1 == i2))
      // Symbolic semantics.
      case (SymVal.AtomicVar(id), SymVal.BigInt(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.BigInt), SmtExpr.BigInt(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.BigInt), SmtExpr.BigInt(i2)) :: pc0, SymVal.False)
      )
      case (SymVal.BigInt(i2), SymVal.AtomicVar(id)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.BigInt), SmtExpr.BigInt(i2)) :: pc0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.BigInt(i2), SmtExpr.Var(id, Type.BigInt)) :: pc0, SymVal.False)
      )

      /**
        * Str.
        */
      case (SymVal.Str(s1), SymVal.Str(s2)) => lift(pc0, toBool(s1 == s2))

      /**
        * Tag.
        */
      case (SymVal.Tag(tag1, v1), SymVal.Tag(tag2, v2)) =>
        if (tag1 == tag2) {
          val innerTpe = tpe.asInstanceOf[Type.Enum]
          eq(pc0, v1, v2, innerTpe.cases(tag1).tpe)
        } else {
          lift(pc0, SymVal.False)
        }

      /**
        * Tuple.
        */
      case (SymVal.Tuple(elms1), SymVal.Tuple(elms2)) =>
        def visit(pc: PathConstraint, elms: List[(SymVal, SymVal)], types: List[Type]): List[(PathConstraint, SymVal)] = (elms, types) match {
          case (Nil, Nil) => lift(pc0, SymVal.True)
          case (Nil, _) => throw InternalCompilerException(s"Type Error: Mismatched tuple.")
          case (_, Nil) => throw InternalCompilerException(s"Type Error: Mismatched tuple.")
          case ((e1, e2) :: es, t :: ts) => eq(pc, e1, e2, t) flatMap {
            case (pc1, SymVal.AtomicVar(id)) => visit(SmtExpr.Var(id, t) :: pc1, es, ts)
            case (pc1, SymVal.True) => visit(pc1, es, ts)
            case (pc1, SymVal.False) => lift(pc1, SymVal.False)
            case (_, v) => throw InternalCompilerException(s"Type Error: Unexpected value '$v'.")
          }
        }
        val elms = elms1 zip elms2
        val types = tpe.asInstanceOf[Type.Tuple].elms
        visit(pc0, elms, types)

      case _ => throw InternalCompilerException(s"Unexpected values: '$x' and '$y'.")
    }

    /**
      * Evaluates the expressions `x` and `y` under the path constraint `pc` and environment `env0`.
      *
      * Evaluates `x` first and then `y` second.
      */
    def eval2(pc0: PathConstraint, x: Expression, y: Expression, env0: Environment): List[(PathConstraint, (SymVal, SymVal))] =
      eval(pc0, x, env0) flatMap {
        case (pcx, vx) => eval(pcx, y, env0) map {
          case (pcy, vy) => pcy -> ((vx, vy))
        }
      }

    /**
      * Evaluates the list of expressions `xs` under the path constraint `pc` and environment `env0`.
      *
      * Evaluates from left to right.
      */
    def evaln(pc0: PathConstraint, xs: Traversable[Expression], env0: Environment): List[(PathConstraint, List[SymVal])] = {
      /*
       * Local visitor.
       */
      def visit(pc: PathConstraint, xs: List[Expression], env: Environment): List[(PathConstraint, List[SymVal])] = xs match {
        case Nil => List((pc, Nil))
        case r :: rs => eval(pc, r, env) flatMap {
          case (pc1, v) => visit(pc1, rs, env) map {
            case (pc2, vs) => (pc2, v :: vs)
          }
        }
      }

      visit(pc0, xs.toList, env0)
    }

    eval(Nil, exp0, env0)
  }

  /**
    * Returns the symbolic value corresponding to the given boolean `b`.
    */
  private def toBool(b: Boolean): SymVal =
    if (b) SymVal.True else SymVal.False

  /**
    * Converts the given value `v` to an expression.
    */
  def toIntExpr(v: SymVal, tpe: Type): SmtExpr = (v, tpe) match {
    case (SymVal.AtomicVar(id), _) => SmtExpr.Var(id, tpe)
    case (SymVal.Int8(i), Type.Int8) => SmtExpr.Int8(i)
    case (SymVal.Int16(i), Type.Int16) => SmtExpr.Int16(i)
    case (SymVal.Int32(i), Type.Int32) => SmtExpr.Int32(i)
    case (SymVal.Int64(i), Type.Int64) => SmtExpr.Int64(i)
    case (SymVal.BigInt(i), Type.BigInt) => SmtExpr.BigInt(i)
    case _ => throw InternalCompilerException(s"Unexpected value: '$v' of type '$tpe'.")
  }

  /**
    * Returns the zero number corresponding to the given type `tpe`.
    */
  private def zeroOf(tpe: Type): SmtExpr = tpe match {
    case Type.Int8 => SmtExpr.Int8(0)
    case Type.Int16 => SmtExpr.Int16(0)
    case Type.Int32 => SmtExpr.Int32(0)
    case Type.Int64 => SmtExpr.Int64(0)
    case Type.BigInt => SmtExpr.BigInt(java.math.BigInteger.ZERO)
    case _ => throw InternalCompilerException(s"Unexpected non-numeric type '$tpe'.")
  }

}
