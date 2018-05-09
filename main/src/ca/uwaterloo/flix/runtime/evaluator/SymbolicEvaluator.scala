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

import java.math.BigInteger

import ca.uwaterloo.flix.api.{HoleException, MatchException, SwitchException, UserException}
import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException}

/**
  * Symbolic evaluator that supports symbolic values and collects path constraints.
  */
object SymbolicEvaluator {

  // TODO: Turn these into case classes.

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
    * An environment is a map from variable symbols to symbolic values.
    */
  type Environment = Map[Symbol.VarSym, SymVal]

  /**
    * The type of label environments.
    *
    * A label environment is a map from label symbols to expressions.
    */
  type LabelEnv = Map[Symbol.LabelSym, Expression]

  /**
    * The type of instantiated quantified variables.
    *
    * A map from symbolic variables to symbolic values.
    */
  case class Quantifiers(m: Map[Symbol.VarSym, SymVal]) {
    def +(pair: (Symbol.VarSym, SymVal)): Quantifiers = Quantifiers(m + pair)
  }

  /**
    * The type of contexts.
    *
    * A context is a list of (path constraint, symbolic value) pairs.
    * Each pair corresponds to one execution path through the program.
    */
  type Context = List[(PathConstraint, Quantifiers, SymVal)]

  /**
    * The type of enumerators.
    *
    * A function from a (symbol, type)-pair to a a list of symbolic values.
    */
  type Enumerator = (Symbol.VarSym, Type) => List[SymVal]

  /**
    * Evaluates the given expression `exp0` under the given environment `env0`.
    */
  def eval(exp0: Expression, env0: Environment, lenv0: LabelEnv, enumerator: Enumerator, root: ExecutableAst.Root)(implicit genSym: GenSym): Context = {
    /*
      * Local visitor.
      */
    def eval(pc0: PathConstraint, exp0: Expression, env0: Environment, lenv0: LabelEnv, qua0: Quantifiers)(implicit genSym: GenSym): Context = exp0 match {
      /**
        * Unit.
        */
      case Expression.Unit => lift(pc0, qua0, SymVal.Unit)

      /**
        * True.
        */
      case Expression.True => lift(pc0, qua0, SymVal.True)

      /**
        * False.
        */
      case Expression.False => lift(pc0, qua0, SymVal.False)

      /**
        * Char.
        */
      case Expression.Char(lit) => lift(pc0, qua0, SymVal.Char(lit))

      /**
        * Float32.
        */
      case Expression.Float32(lit) => lift(pc0, qua0, SymVal.Float32(lit))

      /**
        * Float64.
        */
      case Expression.Float64(lit) => lift(pc0, qua0, SymVal.Float64(lit))

      /**
        * Int8.
        */
      case Expression.Int8(lit) => lift(pc0, qua0, SymVal.Int8(lit))

      /**
        * Int16.
        */
      case Expression.Int16(lit) => lift(pc0, qua0, SymVal.Int16(lit))

      /**
        * Int32.
        */
      case Expression.Int32(lit) => lift(pc0, qua0, SymVal.Int32(lit))

      /**
        * Int64.
        */
      case Expression.Int64(lit) => lift(pc0, qua0, SymVal.Int64(lit))

      /**
        * BigInt.
        */
      case Expression.BigInt(lit) => lift(pc0, qua0, SymVal.BigInt(lit))

      /**
        * Str.
        */
      case Expression.Str(lit) => lift(pc0, qua0, SymVal.Str(lit))

      /**
        * Local Variable.
        */
      case Expression.Var(sym, tpe, loc) => lift(pc0, qua0, env0(sym))

      /**
        * Closure.
        */
      case Expression.Closure(sym, freeVars, _, _, _) =>
        // Save the values of the free variables in a list.
        // When the closure is called, these values will be provided at the beginning of the argument list.
        val bindings = freeVars.map(f => env0(f.sym))
        // Construct the closure.
        val clo = SymVal.Closure(sym, bindings.toArray)
        lift(pc0, qua0, clo)

      /**
        * Apply Closure.
        */
      case Expression.ApplyClo(exp, args, _, _) =>
        invokeClo(pc0, exp, args, env0, lenv0, qua0)

      /**
        * Apply Def.
        */
      case Expression.ApplyDef(sym, args, _, _) =>
        invokeDef(pc0, sym, args, env0, lenv0, qua0)

      /**
        * Apply Eff.
        */
      case Expression.ApplyEff(sym, args, _, _) => ??? // TODO

      /**
        * Apply Closure Tail.
        */
      case Expression.ApplyCloTail(exp, args, _, _) =>
        invokeClo(pc0, exp, args, env0, lenv0, qua0)

      /**
        * Apply Def Tail.
        */
      case Expression.ApplyDefTail(sym, args, _, _) =>
        invokeDef(pc0, sym, args, env0, lenv0, qua0)

      /**
        * Apply Eff Tail.
        */
      case Expression.ApplyEffTail(sym, args, _, _) => ??? // TODO

      /**
        * Apply Self Tail.
        */
      case Expression.ApplySelfTail(sym, _, args, _, _) =>
        invokeDef(pc0, sym, args, env0, lenv0, qua0)


      /**
        * Unary.
        */
      case Expression.Unary(sop, op, exp, _, _) =>
        eval(pc0, exp, env0, lenv0, qua0) flatMap {
          case (pc, qua, v) => op match {
            /**
              * Unary Not.
              */
            case UnaryOperator.LogicalNot => v match {
              // Concrete semantics.
              case SymVal.True => lift(pc, qua, SymVal.False)
              case SymVal.False => lift(pc, qua, SymVal.True)

              // Symbolic Semantics.
              case SymVal.AtomicVar(id, tpe) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, qua, SymVal.False),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, qua, SymVal.True)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
            }

            /**
              * Unary Plus.
              */
            case UnaryOperator.Plus => lift(pc, qua, v)

            /**
              * Unary Minus.
              */
            case UnaryOperator.Minus => v match {
              // Concrete semantics.
              case SymVal.Int8(i) => lift(pc, qua, SymVal.Int8((-i).toByte))
              case SymVal.Int16(i) => lift(pc, qua, SymVal.Int16((-i).toShort))
              case SymVal.Int32(i) => lift(pc, qua, SymVal.Int32(-i))
              case SymVal.Int64(i) => lift(pc, qua, SymVal.Int64(-i))
              case SymVal.BigInt(i) => lift(pc, qua, SymVal.BigInt(i.negate))

              // Symbolic semantics.
              case SymVal.AtomicVar(id, tpe) =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Minus(zeroOf(exp.tpe), SmtExpr.Var(id, exp.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, tpe))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
            }

            /**
              * Unary Bitwise Negate.
              */
            case UnaryOperator.BitwiseNegate => v match {
              // Concrete semantics
              case SymVal.Int8(i) => lift(pc, qua, SymVal.Int8((~i).toByte))
              case SymVal.Int16(i) => lift(pc, qua, SymVal.Int16((~i).toShort))
              case SymVal.Int32(i) => lift(pc, qua, SymVal.Int32(~i))
              case SymVal.Int64(i) => lift(pc, qua, SymVal.Int64(~i))
              case SymVal.BigInt(i) => lift(pc, qua, SymVal.BigInt(i.not))

              // Symbolic semantics
              case SymVal.AtomicVar(id, tpe) =>
                // NB: Must subtract one from the result of SmtExpr.BitwiseNegate since Z3 performs two's complement and adds one.
                val one = tpe match {
                  case Type.Int8 => SmtExpr.Int8(1)
                  case Type.Int16 => SmtExpr.Int16(1)
                  case Type.Int32 => SmtExpr.Int32(1)
                  case Type.Int64 => SmtExpr.Int64(1)
                  case Type.BigInt => SmtExpr.BigInt(BigInteger.ONE)
                  case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
                }
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Minus(SmtExpr.BitwiseNegate(SmtExpr.Var(id, exp.tpe)), one)) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, tpe))

              case _ => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
            }

          }
        }

      /**
        * Binary.
        */
      case Expression.Binary(sop, op, exp1, exp2, _, _) =>
        eval2(pc0, exp1, exp2, env0, lenv0, qua0) flatMap {
          case (pc, qua, (v1, v2)) => op match {

            /**
              * Plus.
              */
            case BinaryOperator.Plus => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8((i1 + i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16((i1 + i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 + i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(i1 + i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, SymVal.BigInt(i1 add i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Plus(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Minus.
              */
            case BinaryOperator.Minus => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8((i1 - i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16((i1 - i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 - i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(i1 - i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, SymVal.BigInt(i1 subtract i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Minus(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Times.
              */
            case BinaryOperator.Times => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8((i1 * i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16((i1 * i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 * i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(i1 * i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, SymVal.BigInt(i1 multiply i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Times(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Divide.
              */
            case BinaryOperator.Divide => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8((i1 / i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16((i1 / i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 / i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(i1 / i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, SymVal.BigInt(i1 divide i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Divide(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Modulo.
              */
            case BinaryOperator.Modulo => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8((i1 % i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16((i1 % i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 % i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(i1 % i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, SymVal.BigInt(i1 remainder i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Modulo(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Exponentiate.
              */
            case BinaryOperator.Exponentiate => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8(Math.pow(i1, i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16(Math.pow(i1, i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(Math.pow(i1, i2).toInt))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(Math.pow(i1, i2).toLong))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => throw InternalCompilerException(s"Type Error: BigInt does not support Exponentiate.")

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.Exponentiate(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Less.
              */
            case BinaryOperator.Less => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, toBool(i1 < i2))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, toBool(i1 < i2))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, toBool(i1 < i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, toBool(i1 < i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, toBool(i1.compareTo(i2) < 0))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, Type.Bool), SmtExpr.Less(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * LessEqual.
              */
            case BinaryOperator.LessEqual => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, toBool(i1 <= i2))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, toBool(i1 <= i2))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, toBool(i1 <= i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, toBool(i1 <= i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, toBool(i1.compareTo(i2) <= 0))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, Type.Bool), SmtExpr.LessEqual(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Greater.
              */
            case BinaryOperator.Greater => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, toBool(i1 > i2))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, toBool(i1 > i2))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, toBool(i1 > i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, toBool(i1 > i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, toBool(i1.compareTo(i2) > 0))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, Type.Bool), SmtExpr.Greater(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * GreaterEqual.
              */
            case BinaryOperator.GreaterEqual => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, toBool(i1 >= i2))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, toBool(i1 >= i2))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, toBool(i1 >= i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, toBool(i1 >= i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, toBool(i1.compareTo(i2) >= 0))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, Type.Bool), SmtExpr.GreaterEqual(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Equal.
              */
            case BinaryOperator.Equal => eq(pc, qua, v1, v2)

            /**
              * Not Equal.
              */
            case BinaryOperator.NotEqual => eq(pc, qua, v1, v2).flatMap {
              case (pc1, qua1, SymVal.True) => lift(pc1, qua1, SymVal.False)
              case (pc1, qua1, SymVal.False) => lift(pc1, qua1, SymVal.True)
              case (_, _, v) => throw InternalCompilerException(s"Type Error: Unexpected value:'$v'.")
            }

            /**
              * Logical And.
              */
            case BinaryOperator.LogicalAnd => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, qua, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, qua, SymVal.False)
              case (SymVal.False, SymVal.True) => lift(pc, qua, SymVal.False)
              case (SymVal.False, SymVal.False) => lift(pc, qua, SymVal.False)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id, _)) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, qua, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, qua, SymVal.False)
              )
              case (SymVal.AtomicVar(id, _), SymVal.True) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, qua, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, qua, SymVal.False)
              )

              case (SymVal.False, SymVal.AtomicVar(id, _)) => lift(pc, qua, SymVal.False)
              case (SymVal.AtomicVar(id, _), SymVal.False) => lift(pc, qua, SymVal.False)

              case (SymVal.AtomicVar(id1, _), SymVal.AtomicVar(id2, _)) => List(
                (SmtExpr.LogicalAnd(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool)) :: pc, qua, SymVal.True),
                (SmtExpr.Not(SmtExpr.LogicalAnd(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool))) :: pc, qua, SymVal.False)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 && $v2'.")
            }

            /**
              * Logical Or.
              */
            case BinaryOperator.LogicalOr => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.True, SymVal.True) => lift(pc, qua, SymVal.True)
              case (SymVal.True, SymVal.False) => lift(pc, qua, SymVal.True)
              case (SymVal.False, SymVal.True) => lift(pc, qua, SymVal.True)
              case (SymVal.False, SymVal.False) => lift(pc, qua, SymVal.False)

              // Symbolic semantics.
              case (SymVal.True, SymVal.AtomicVar(id, _)) => lift(pc, qua, SymVal.True)
              case (SymVal.AtomicVar(id, _), SymVal.True) => lift(pc, qua, SymVal.True)

              case (SymVal.False, SymVal.AtomicVar(id, _)) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, qua, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, qua, SymVal.False)
              )
              case (SymVal.AtomicVar(id, _), SymVal.False) => List(
                (SmtExpr.Var(id, Type.Bool) :: pc, qua, SymVal.True),
                (SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, qua, SymVal.False)
              )

              case (SymVal.AtomicVar(id1, _), SymVal.AtomicVar(id2, _)) => List(
                (SmtExpr.LogicalOr(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool)) :: pc, qua, SymVal.True),
                (SmtExpr.Not(SmtExpr.LogicalOr(SmtExpr.Var(id1, Type.Bool), SmtExpr.Var(id2, Type.Bool))) :: pc, qua, SymVal.False)
              )

              case _ => throw InternalCompilerException(s"Type Error: Unexpected expression: '$v1 || $v2'.")
            }

            /**
              * Bitwise And.
              */
            case BinaryOperator.BitwiseAnd => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8((i1 & i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16((i1 & i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 & i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(i1 & i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, SymVal.BigInt(i1 and i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseAnd(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Bitwise Or.
              */
            case BinaryOperator.BitwiseOr => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8((i1 | i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16((i1 | i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 | i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(i1 | i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, SymVal.BigInt(i1 or i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseOr(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Bitwise Xor.
              */
            case BinaryOperator.BitwiseXor => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc, qua, SymVal.Int8((i1 ^ i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc, qua, SymVal.Int16((i1 ^ i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 ^ i2))
              case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc, qua, SymVal.Int64(i1 ^ i2))
              case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc, qua, SymVal.BigInt(i1 xor i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseXor(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Bitwise Left Shift.
              */
            case BinaryOperator.BitwiseLeftShift => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int8((i1 << i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int16((i1 << i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 << i2))
              case (SymVal.Int64(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int64(i1 << i2))
              case (SymVal.BigInt(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.BigInt(i1 shiftLeft i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseLeftShift(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }

            /**
              * Bitwise Right Shift.
              */
            case BinaryOperator.BitwiseRightShift => (v1, v2) match {
              // Concrete semantics.
              case (SymVal.Int8(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int8((i1 >> i2).toByte))
              case (SymVal.Int16(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int16((i1 >> i2).toShort))
              case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int32(i1 >> i2))
              case (SymVal.Int64(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.Int64(i1 >> i2))
              case (SymVal.BigInt(i1), SymVal.Int32(i2)) => lift(pc, qua, SymVal.BigInt(i1 shiftRight i2))

              // Symbolic semantics.
              case _ =>
                val newVar = Symbol.freshVarSym()
                val newPC = SmtExpr.Equal(SmtExpr.Var(newVar, exp0.tpe), SmtExpr.BitwiseRightShift(toIntExpr(v1, exp1.tpe), toIntExpr(v2, exp2.tpe))) :: pc
                lift(newPC, qua, SymVal.AtomicVar(newVar, exp0.tpe))
            }
          }
        }

      /**
        * If-then-else.
        */
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        eval(pc0, exp1, env0, lenv0, qua0) flatMap {
          case (pc, qua, c) => c match {
            case SymVal.True => eval(pc, exp2, env0, lenv0, qua)
            case SymVal.False => eval(pc, exp3, env0, lenv0, qua)
            case SymVal.AtomicVar(id, _) =>
              // Evaluate both branches under different path constraints.
              val consequent = eval(SmtExpr.Var(id, Type.Bool) :: pc, exp2, env0, lenv0, qua)
              val alternative = eval(SmtExpr.Not(SmtExpr.Var(id, Type.Bool)) :: pc, exp3, env0, lenv0, qua)
              consequent ++ alternative
            case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
          }
        }

      /**
        * Block.
        */
      case Expression.Branch(exp, branches, tpe, loc) => eval(pc0, exp, env0, lenv0, qua0)

      /**
        * Jump.
        */
      case Expression.JumpTo(sym, tpe, loc) => lenv0.get(sym) match {
        case None => throw InternalCompilerException(s"Unknown label: '$sym' in label environment ${lenv0.mkString(" ,")}.")
        case Some(e) => eval(pc0, e, env0, lenv0, qua0)
      }

      /**
        * Let-binding.
        */
      case Expression.Let(sym, exp1, exp2, _, _) =>
        eval(pc0, exp1, env0, lenv0, qua0) flatMap {
          case (pc, qua, v1) =>
            // Bind the variable to the value of `exp1` which is `v1`.
            val newEnv = env0 + (sym -> v1)
            eval(pc, exp2, newEnv, lenv0, qua)
        }

      /**
        * LetRec-binding.
        */
      case Expression.LetRec(sym, exp1, exp2, _, _) => exp1 match {
        case Expression.Closure(ref, freeVars, _, _, _) =>
          // Save the values of the free variables in a list.
          // When the closure is called, these values will be provided at the beginning of the argument list.
          val bindings = Array.ofDim[SymVal](freeVars.length)
          for (freeVar <- freeVars) {
            // A value might be absent from the the environment if it is recursively bound.
            env0.get(freeVar.sym) match {
              case None => // Ok, value probably recursive.
              case Some(v) => bindings(sym.getStackOffset) = v
            }
          }
          // Construct circular closure.
          val clo = SymVal.Closure(ref, bindings)
          bindings(sym.getStackOffset) = clo

          // Return the closure.
          lift(pc0, qua0, clo)
        case _ => throw InternalRuntimeException(s"Expected MkClosureRef expression: '$exp1'")
      }

      /**
        * Is Tag.
        */
      case Expression.Is(sym, tag, exp, _) =>
        eval(pc0, exp, env0, lenv0, qua0) flatMap {
          case (pc, qua, SymVal.Tag(tag2, _)) =>
            if (tag == tag2)
              lift(pc, qua, SymVal.True)
            else
              lift(pc, qua, SymVal.False)
          case (_, _, v) => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * Tag.
        */
      case Expression.Tag(sym, tag, exp, _, _) =>
        eval(pc0, exp, env0, lenv0, qua0) flatMap {
          case (pc, qua, v) => lift(pc, qua, SymVal.Tag(tag, v))
        }

      /**
        * Untag.
        */
      case Expression.Untag(sym, tag, exp, _, _) =>
        eval(pc0, exp, env0, lenv0, qua0) flatMap {
          case (pc, qua, SymVal.Tag(_, v)) => lift(pc, qua, v)
          case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * Tuple.
        */
      case Expression.Tuple(elms, _, _) =>
        evaln(pc0, elms, env0, lenv0, qua0) flatMap {
          case (pc, qua, es) => lift(pc, qua, SymVal.Tuple(es))
        }

      /**
        * Index (into tuple).
        */
      case Expression.Index(base, offset, _, _) =>
        eval(pc0, base, env0, lenv0, qua0) flatMap {
          case (pc, qua, SymVal.Tuple(elms)) => lift(pc, qua, elms(offset))
          case v => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
        }

      /**
        * ArrayLit.
        */
      case Expression.ArrayLit(elms, tpe, loc) => ???

      /**
        * ArrayNew.
        */
      case Expression.ArrayNew(elm, len, tpe, loc) => ???

      /**
        * ArrayLoad.
        */
      case Expression.ArrayLoad(base, index, tpe, loc) => ???

      /**
        * ArrayStore.
        */
      case Expression.ArrayStore(base, index, elm, tpe, loc) => ???

      /**
        * ArrayLength.
        */
      case Expression.ArrayLength(base, tpe, loc) => ???

      /**
        * ArraySlice.
        */
      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???


      /**
        * Reference.
        */
      case Expression.Ref(exp, tpe, loc) => ??? // TODO

      /**
        * Dereference.
        */
      case Expression.Deref(exp, tpe, loc) => ??? // TODO

      /**
        * Assignment.
        */
      case Expression.Assign(exp1, exp2, tpe, loc) => ??? // TODO

      /**
        * HandleWith.
        */
      case Expression.HandleWith(exp, bindings, tpe, loc) => ??? // TODO

      /**
        * Existential Quantifier.
        */
      case e: Expression.Existential => throw InternalCompilerException(s"Unsupported expression: '$e'.") // TODO

      /**
        * Universal Quantifier.
        */
      case Expression.Universal(fparam, exp, _) =>
        // Enumerate the possible symbolic values of the formal parameter.
        enumerator(fparam.sym, fparam.tpe) flatMap {
          case value => eval(pc0, exp, env0 + (fparam.sym -> value), lenv0, qua0 + (fparam.sym -> value))
        }

      /**
        * Native Constructor.
        */
      case Expression.NativeConstructor(constructor, args, tpe, loc) => throw InternalCompilerException("Not yet supported.")

      /**
        * Native Field.
        */
      case Expression.NativeField(field, tpe, loc) => throw InternalCompilerException("Not yet supported.")

      /**
        * Native Method.
        */
      case Expression.NativeMethod(method, args, tpe, loc) => throw InternalCompilerException("Not yet supported.")

      /**
        * User Error.
        */
      case Expression.UserError(tpe, loc) => throw UserException("User Error.", loc)

      /**
        * Hole Error.
        */
      case Expression.HoleError(sym, tpe, loc) => throw HoleException(sym.toString, loc)

      /**
        * Match Error.
        */
      case Expression.MatchError(tpe, loc) => throw MatchException("Match Error.", loc)

      /**
        * Switch Error
        */
      case Expression.SwitchError(tpe, loc) => throw SwitchException("Switch Error", loc)

    }

    /**
      * Returns a context with the value `v` guarded by the path constraint `pc`.
      */
    def lift(pc0: PathConstraint, qua0: Quantifiers, v: SymVal): Context = List((pc0, qua0, v))

    /**
      * Test equality of `x` and `y` (of type `tpe`) under the path constraint `pc0`.
      */
    def eq(pc0: PathConstraint, qua0: Quantifiers, x: SymVal, y: SymVal): Context = (x, y) match {
      /**
        * Variable.
        */
      case (SymVal.AtomicVar(ident1, tpe1), SymVal.AtomicVar(ident2, tpe2)) =>
        assert(tpe1 == tpe2)
        // Equality of two atomic variables is encoded using two path constraints.
        List(
          (SmtExpr.Equal(SmtExpr.Var(ident1, tpe1), SmtExpr.Var(ident2, tpe1)) :: pc0, qua0, SymVal.True),
          (SmtExpr.NotEqual(SmtExpr.Var(ident1, tpe1), SmtExpr.Var(ident2, tpe1)) :: pc0, qua0, SymVal.False)
        )

      /**
        * Unit.
        */
      case (SymVal.Unit, SymVal.Unit) => lift(pc0, qua0, SymVal.True)

      /**
        * True & False.
        */
      case (SymVal.True, SymVal.True) => lift(pc0, qua0, SymVal.True)
      case (SymVal.True, SymVal.False) => lift(pc0, qua0, SymVal.False)
      case (SymVal.False, SymVal.True) => lift(pc0, qua0, SymVal.False)
      case (SymVal.False, SymVal.False) => lift(pc0, qua0, SymVal.True)

      /**
        * Char.
        */
      case (SymVal.Char(c1), SymVal.Char(c2)) => lift(pc0, qua0, toBool(c1 == c2))

      /**
        * Float32
        */
      case (SymVal.Float32(f1), SymVal.Float64(f2)) => lift(pc0, qua0, toBool(f1 == f2))

      /**
        * Float64
        */
      case (SymVal.Float64(f1), SymVal.Float64(f2)) => lift(pc0, qua0, toBool(f1 == f2))

      /**
        * Int8.
        */
      // Concrete semantics.
      case (SymVal.Int8(i1), SymVal.Int8(i2)) => lift(pc0, qua0, toBool(i1 == i2))
      // Symbolic semantics.  
      case (SymVal.AtomicVar(id, _), SymVal.Int8(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int8), SmtExpr.Int8(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.Int8), SmtExpr.Int8(i2)) :: pc0, qua0, SymVal.False)
      )
      case (SymVal.Int8(i2), SymVal.AtomicVar(id, _)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int8), SmtExpr.Int8(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Int8(i2), SmtExpr.Var(id, Type.Int8)) :: pc0, qua0, SymVal.False)
      )

      /**
        * Int16.
        */
      // Concrete semantics.
      case (SymVal.Int16(i1), SymVal.Int16(i2)) => lift(pc0, qua0, toBool(i1 == i2))
      // Symbolic semantics.
      case (SymVal.AtomicVar(id, _), SymVal.Int16(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int16), SmtExpr.Int16(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.Int16), SmtExpr.Int16(i2)) :: pc0, qua0, SymVal.False)
      )
      case (SymVal.Int16(i2), SymVal.AtomicVar(id, _)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int16), SmtExpr.Int16(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Int16(i2), SmtExpr.Var(id, Type.Int16)) :: pc0, qua0, SymVal.False)
      )

      /**
        * Int32.
        */
      // Concrete semantics.
      case (SymVal.Int32(i1), SymVal.Int32(i2)) => lift(pc0, qua0, toBool(i1 == i2))
      // Symbolic semantics.
      case (SymVal.AtomicVar(id, _), SymVal.Int32(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int32), SmtExpr.Int32(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.Int32), SmtExpr.Int32(i2)) :: pc0, qua0, SymVal.False)
      )
      case (SymVal.Int32(i2), SymVal.AtomicVar(id, _)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int32), SmtExpr.Int32(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Int32(i2), SmtExpr.Var(id, Type.Int32)) :: pc0, qua0, SymVal.False)
      )

      /**
        * Int64.
        */
      // Concrete semantics.
      case (SymVal.Int64(i1), SymVal.Int64(i2)) => lift(pc0, qua0, toBool(i1 == i2))
      // Symbolic semantics.
      case (SymVal.AtomicVar(id, _), SymVal.Int64(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int64), SmtExpr.Int64(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.Int64), SmtExpr.Int64(i2)) :: pc0, qua0, SymVal.False)
      )
      case (SymVal.Int64(i2), SymVal.AtomicVar(id, _)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.Int64), SmtExpr.Int64(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Int64(i2), SmtExpr.Var(id, Type.Int64)) :: pc0, qua0, SymVal.False)
      )

      /**
        * BigInt.
        */
      // Concrete semantics.
      case (SymVal.BigInt(i1), SymVal.BigInt(i2)) => lift(pc0, qua0, toBool(i1 == i2))
      // Symbolic semantics.
      case (SymVal.AtomicVar(id, _), SymVal.BigInt(i2)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.BigInt), SmtExpr.BigInt(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.Var(id, Type.BigInt), SmtExpr.BigInt(i2)) :: pc0, qua0, SymVal.False)
      )
      case (SymVal.BigInt(i2), SymVal.AtomicVar(id, _)) => List(
        (SmtExpr.Equal(SmtExpr.Var(id, Type.BigInt), SmtExpr.BigInt(i2)) :: pc0, qua0, SymVal.True),
        (SmtExpr.NotEqual(SmtExpr.BigInt(i2), SmtExpr.Var(id, Type.BigInt)) :: pc0, qua0, SymVal.False)
      )

      /**
        * Str.
        */
      case (SymVal.Str(s1), SymVal.Str(s2)) => lift(pc0, qua0, toBool(s1 == s2))

      /**
        * Tag.
        */
      case (SymVal.Tag(tag1, v1), SymVal.Tag(tag2, v2)) =>
        if (tag1 == tag2) {
          eq(pc0, qua0, v1, v2)
        } else {
          lift(pc0, qua0, SymVal.False)
        }

      /**
        * Tuple.
        */
      case (SymVal.Tuple(elms1), SymVal.Tuple(elms2)) =>
        def visit(pc: PathConstraint, qua: Quantifiers, elms: List[(SymVal, SymVal)]): List[(PathConstraint, Quantifiers, SymVal)] = elms match {
          case Nil => lift(pc0, qua, SymVal.True)
          case (e1, e2) :: es => eq(pc, qua, e1, e2) flatMap {
            case (pc1, qua1, SymVal.AtomicVar(id, tpe)) => visit(SmtExpr.Var(id, tpe) :: pc1, qua1, es)
            case (pc1, qua1, SymVal.True) => visit(pc1, qua1, es)
            case (pc1, qua1, SymVal.False) => lift(pc1, qua1, SymVal.False)
            case (_, _, v) => throw InternalCompilerException(s"Type Error: Unexpected value '$v'.")
          }
        }

        val elms = elms1 zip elms2
        visit(pc0, qua0, elms)

      case _ => throw InternalCompilerException(s"Unexpected values: '$x' and '$y'.")
    }

    /**
      * Evaluates the expressions `x` and `y` under the path constraint `pc` and environment `env0`.
      *
      * Evaluates `x` first and then `y` second.
      */
    def eval2(pc0: PathConstraint, x: Expression, y: Expression, env0: Environment, lenv0: LabelEnv, qua0: Quantifiers): List[(PathConstraint, Quantifiers, (SymVal, SymVal))] =
      eval(pc0, x, env0, lenv0, qua0) flatMap {
        case (pcx, quax, vx) => eval(pcx, y, env0, lenv0, quax) map {
          case (pcy, quay, vy) => (pcy, quay, (vx, vy))
        }
      }

    /**
      * Evaluates the list of expressions `xs` under the path constraint `pc0` and environment `env0`.
      *
      * Evaluates from left to right.
      */
    def evaln(pc0: PathConstraint, xs: Traversable[Expression], env0: Environment, lenv0: LabelEnv, qua0: Quantifiers): List[(PathConstraint, Quantifiers, List[SymVal])] = {
      /*
       * Local visitor.
       */
      def visit(pc: PathConstraint, xs: List[Expression], env: Environment, lenv: LabelEnv, qua: Quantifiers): List[(PathConstraint, Quantifiers, List[SymVal])] = xs match {
        case Nil => List((pc, qua, Nil))
        case r :: rs => eval(pc, r, env, lenv, qua) flatMap {
          case (pc1, qua1, v) => visit(pc1, rs, env, lenv, qua1) map {
            case (pc2, qua2, vs) => (pc2, qua2, v :: vs)
          }
        }
      }

      visit(pc0, xs.toList, env0, Map.empty, qua0)
    }

    /**
      * Invokes the given closure expression `exp` with `args` under the path constraint `pc0` and environment `env0`.
      */
    def invokeClo(pc0: PathConstraint, exp: Expression, args: List[Expression], env0: Environment, lenv0: LabelEnv, qua0: Quantifiers): Context = {
      // Evaluate the closure.
      eval(pc0, exp, env0, lenv0, qua0) flatMap {
        case (pc, qua, SymVal.Closure(sym, bindings)) =>
          // Lookup the definition
          val defn = root.defs(sym)
          // Evaluate all the arguments.
          evaln(pc, args, env0, lenv0, qua) flatMap {
            case (pc1, qua1, actuals) =>
              // Construct the environment
              val newArgs = bindings ++ actuals
              val newEnv = (defn.formals zip newArgs).foldLeft(Map.empty: Environment) {
                case (macc, (formal, actual)) => macc + (formal.sym -> actual)
              }
              eval(pc1, defn.exp, newEnv, Map.empty, qua1)
          }
        case (_, _, v) => throw InternalCompilerException(s"Type Error: Unexpected value: '$v'.")
      }
    }

    /**
      * Invokes the given definition `sym`  with `args` under the path constraint `pc0` and environment `env0`.
      */
    def invokeDef(pc0: PathConstraint, sym: Symbol.DefnSym, args: List[Expression], env0: Environment, lenv0: LabelEnv, qua0: Quantifiers): Context = {
      // Lookup the reference.
      val defn = root.defs(sym)
      // Evaluate all the arguments.
      evaln(pc0, args, env0, lenv0, qua0) flatMap {
        case (pc, qua, as) =>
          // Bind the actual arguments to the formal variables.
          val newEnv = (defn.formals zip as).foldLeft(Map.empty: Environment) {
            case (macc, (formal, actual)) => macc + (formal.sym -> actual)
          }
          // Evaluate the body under the new environment.
          eval(pc, defn.exp, newEnv, Map.empty, qua)
      }
    }

    // Start
    eval(Nil, exp0, env0, Map.empty, Quantifiers(Map.empty))
  }

  /**
    * Returns the symbolic value corresponding to the given boolean `b`.
    */
  private def toBool(b: Boolean): SymVal = if (b) SymVal.True else SymVal.False

  /**
    * Converts the given value `v` to an expression.
    */
  def toIntExpr(v: SymVal, tpe: Type): SmtExpr = (v, tpe) match {
    case (SymVal.AtomicVar(id, _), _) => SmtExpr.Var(id, tpe)
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
