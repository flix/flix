/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

/**
  * A phase that simplifies the TypedAst by elimination of pattern matching and other rewritings.
  */
object Simplifier extends Phase[TypedAst.Root, SimplifiedAst.Root] {

  type TopLevel = mutable.Map[Symbol.DefnSym, SimplifiedAst.Def]

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    // Put GenSym into the implicit scope.
    implicit val _ = flix.genSym

    // Measure elapsed time.
    val t = System.nanoTime()

    // A mutable map to contain fresh top-level definitions.
    val toplevel: TopLevel = mutable.Map.empty

    /**
      * Translates the given `constraint0` to the SimplifiedAst.
      */
    def visitConstraint(constraint0: TypedAst.Constraint): SimplifiedAst.Constraint = {
      val head = Predicate.Head.simplify(constraint0.head, constraint0.cparams, toplevel)
      val body = constraint0.body.map(p => Predicate.Body.simplify(p, constraint0.cparams, toplevel))
      val cparams = constraint0.cparams.map {
        case TypedAst.ConstraintParam.HeadParam(sym, tpe, loc) => SimplifiedAst.ConstraintParam.HeadParam(sym, tpe, loc)
        case TypedAst.ConstraintParam.RuleParam(sym, tpe, loc) => SimplifiedAst.ConstraintParam.RuleParam(sym, tpe, loc)
      }

      SimplifiedAst.Constraint(cparams, head, body)
    }

    /**
      * Translates the given definition `def0` to the SimplifiedAst.
      */
    def visitDef(def0: TypedAst.Def): SimplifiedAst.Def = {
      val fs = def0.fparams.map(simplify)
      val exp = Expression.simplify(def0.exp)
      SimplifiedAst.Def(def0.ann, def0.mod, def0.sym, fs, exp, isSynthetic = false, def0.tpe, def0.loc)
    }

    /**
      * Translates the given `index0` to the SimplifiedAst.
      */
    def visitIndex(index0: TypedAst.Index): SimplifiedAst.Index =
      SimplifiedAst.Index(index0.sym, index0.indexes, index0.loc)

    /**
      * Translates the given `lattice0` to the SimplifiedAst.
      */
    def visitLattice(lattice0: TypedAst.Lattice): SimplifiedAst.Lattice = lattice0 match {
      case TypedAst.Lattice(tpe, bot0, top0, equ0, leq0, lub0, glb0, loc) =>
        val bot = Expression.simplify(bot0)
        val top = Expression.simplify(top0)
        val equ = Expression.simplify(equ0)
        val leq = Expression.simplify(leq0)
        val lub = Expression.simplify(lub0)
        val glb = Expression.simplify(glb0)
        SimplifiedAst.Lattice(tpe, bot, top, equ, leq, lub, glb, loc)
    }

    /**
      * Translates the given `stratum0` to the SimplifiedAst.
      */
    def visitStratum(stratum0: TypedAst.Stratum): SimplifiedAst.Stratum =
      SimplifiedAst.Stratum(stratum0.constraints.map(c => visitConstraint(c)))

    /**
      * Translates the given `table0` to the SimplifiedAst.
      */
    def visitTable(table0: TypedAst.Table): SimplifiedAst.Table = table0 match {
      case TypedAst.Table.Relation(doc, symbol, attributes, loc) =>
        SimplifiedAst.Table.Relation(symbol, attributes.map(Simplifier.simplify), loc)
      case TypedAst.Table.Lattice(doc, name, keys, value, loc) =>
        SimplifiedAst.Table.Lattice(name, keys.map(Simplifier.simplify), Simplifier.simplify(value), loc)
    }


    val defns = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map {
      case (k, TypedAst.Enum(doc, sym, cases0, sc, loc)) =>
        val cases = cases0 map {
          case (tag, TypedAst.Case(enum, tagName, tpe)) => tag -> SimplifiedAst.Case(enum, tagName, tpe)
        }
        k -> SimplifiedAst.Enum(sym, cases, loc)
    }
    val lattices = root.lattices.map { case (k, v) => k -> visitLattice(v) }
    val collections = root.tables.map { case (k, v) => k -> visitTable(v) }
    val indexes = root.indexes.map { case (k, v) => k -> visitIndex(v) }
    val strata = root.strata.map(visitStratum)
    val properties = root.properties.map { p => simplify(p) }
    val specialOps = root.specialOps
    val reachable = root.reachable
    val time = root.time

    val e = System.nanoTime() - t
    SimplifiedAst.Root(defns ++ toplevel, enums, lattices, collections, indexes, strata, properties, specialOps, reachable, time.copy(simplifier = e)).toSuccess
  }

  object Expression {
    def simplify(tast: TypedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Expression = tast match {
      case TypedAst.Expression.Wild(tpe, eff, loc) => ??? // TODO
      case TypedAst.Expression.Unit(loc) => SimplifiedAst.Expression.Unit
      case TypedAst.Expression.True(loc) => SimplifiedAst.Expression.True
      case TypedAst.Expression.False(loc) => SimplifiedAst.Expression.False
      case TypedAst.Expression.Char(lit, loc) => SimplifiedAst.Expression.Char(lit)
      case TypedAst.Expression.Float32(lit, loc) => SimplifiedAst.Expression.Float32(lit)
      case TypedAst.Expression.Float64(lit, loc) => SimplifiedAst.Expression.Float64(lit)
      case TypedAst.Expression.Int8(lit, loc) => SimplifiedAst.Expression.Int8(lit)
      case TypedAst.Expression.Int16(lit, loc) => SimplifiedAst.Expression.Int16(lit)
      case TypedAst.Expression.Int32(lit, loc) => SimplifiedAst.Expression.Int32(lit)
      case TypedAst.Expression.Int64(lit, loc) => SimplifiedAst.Expression.Int64(lit)
      case TypedAst.Expression.BigInt(lit, loc) => SimplifiedAst.Expression.BigInt(lit)
      case TypedAst.Expression.Str(lit, loc) => SimplifiedAst.Expression.Str(lit)
      case TypedAst.Expression.Var(sym, tpe, eff, loc) => SimplifiedAst.Expression.Var(sym, tpe, loc)
      case TypedAst.Expression.Def(sym, tpe, eff, loc) => SimplifiedAst.Expression.Def(sym, tpe, loc)
      case TypedAst.Expression.Hook(hook, tpe, eff, loc) => SimplifiedAst.Expression.Hook(hook, tpe, loc)
      case TypedAst.Expression.Lambda(args, body, tpe, eff, loc) =>
        SimplifiedAst.Expression.Lambda(args map Simplifier.simplify, simplify(body), tpe, loc)
      case TypedAst.Expression.Apply(e, args, tpe, eff, loc) =>
        SimplifiedAst.Expression.Apply(simplify(e), args map simplify, tpe, loc)

      case TypedAst.Expression.Unary(op, e, tpe, eff, loc) =>
        /*
         * Special Case 1: Unary Plus.
         */
        (op, e.tpe) match {
          case (UnaryOperator.Plus, _) =>
            // A unary plus has no semantic effect.
            return simplify(e)
          case _ => // fallthrough
        }

        /*
         * Compute the semantic operator based on types.
         */
        val sop = op match {
          case UnaryOperator.LogicalNot => SemanticOperator.BoolOp.Not
          case UnaryOperator.BitwiseNegate => e.tpe match {
            case Type.Int8 => SemanticOperator.Int8Op.Not
            case Type.Int16 => SemanticOperator.Int16Op.Not
            case Type.Int32 => SemanticOperator.Int32Op.Not
            case Type.Int64 => SemanticOperator.Int64Op.Not
            case Type.BigInt => SemanticOperator.BigIntOp.Not
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case UnaryOperator.Plus =>
            throw InternalCompilerException(s"Impossible.")
          case UnaryOperator.Minus => e.tpe match {
            case Type.Float32 => SemanticOperator.Float32Op.Neg
            case Type.Float64 => SemanticOperator.Float64Op.Neg
            case Type.Int8 => SemanticOperator.Int8Op.Neg
            case Type.Int16 => SemanticOperator.Int16Op.Neg
            case Type.Int32 => SemanticOperator.Int32Op.Neg
            case Type.Int64 => SemanticOperator.Int64Op.Neg
            case Type.BigInt => SemanticOperator.BigIntOp.Neg
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
        }

        SimplifiedAst.Expression.Unary(sop, op, simplify(e), tpe, loc)

      case TypedAst.Expression.Binary(op, e1, e2, tpe, eff, loc) =>

        /*
         * Special Case 1: Unit
         */
        (op, e1.tpe, e2.tpe) match {
          case (BinaryOperator.Equal, Type.Unit, Type.Unit) =>
            // Unit is always equal to itself.
            return SimplifiedAst.Expression.True
          case (BinaryOperator.NotEqual, Type.Unit, Type.Unit) =>
            // Unit is never not equal to itself.
            return SimplifiedAst.Expression.False
          case _ => // fallthrough
        }

        /*
         * Compute the semantic operator based on types.
         */
        val sop = op match {
          case BinaryOperator.Plus => e1.tpe match {
            case Type.Float32 => SemanticOperator.Float32Op.Add
            case Type.Float64 => SemanticOperator.Float64Op.Add
            case Type.Int8 => SemanticOperator.Int8Op.Add
            case Type.Int16 => SemanticOperator.Int16Op.Add
            case Type.Int32 => SemanticOperator.Int32Op.Add
            case Type.Int64 => SemanticOperator.Int64Op.Add
            case Type.BigInt => SemanticOperator.BigIntOp.Add
            case Type.Str => SemanticOperator.StringOp.Concat
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Minus => e1.tpe match {
            case Type.Float32 => SemanticOperator.Float32Op.Sub
            case Type.Float64 => SemanticOperator.Float64Op.Sub
            case Type.Int8 => SemanticOperator.Int8Op.Sub
            case Type.Int16 => SemanticOperator.Int16Op.Sub
            case Type.Int32 => SemanticOperator.Int32Op.Sub
            case Type.Int64 => SemanticOperator.Int64Op.Sub
            case Type.BigInt => SemanticOperator.BigIntOp.Sub
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Times => e1.tpe match {
            case Type.Float32 => SemanticOperator.Float32Op.Mul
            case Type.Float64 => SemanticOperator.Float64Op.Mul
            case Type.Int8 => SemanticOperator.Int8Op.Mul
            case Type.Int16 => SemanticOperator.Int16Op.Mul
            case Type.Int32 => SemanticOperator.Int32Op.Mul
            case Type.Int64 => SemanticOperator.Int64Op.Mul
            case Type.BigInt => SemanticOperator.BigIntOp.Mul
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Divide => e1.tpe match {
            case Type.Float32 => SemanticOperator.Float32Op.Div
            case Type.Float64 => SemanticOperator.Float64Op.Div
            case Type.Int8 => SemanticOperator.Int8Op.Div
            case Type.Int16 => SemanticOperator.Int16Op.Div
            case Type.Int32 => SemanticOperator.Int32Op.Div
            case Type.Int64 => SemanticOperator.Int64Op.Div
            case Type.BigInt => SemanticOperator.BigIntOp.Div
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Modulo => e1.tpe match {
            case Type.Float32 => SemanticOperator.Float32Op.Rem
            case Type.Float64 => SemanticOperator.Float64Op.Rem
            case Type.Int8 => SemanticOperator.Int8Op.Rem
            case Type.Int16 => SemanticOperator.Int16Op.Rem
            case Type.Int32 => SemanticOperator.Int32Op.Rem
            case Type.Int64 => SemanticOperator.Int64Op.Rem
            case Type.BigInt => SemanticOperator.BigIntOp.Rem
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Exponentiate => e1.tpe match {
            case Type.Float32 => SemanticOperator.Float32Op.Exp
            case Type.Float64 => SemanticOperator.Float64Op.Exp
            case Type.Int8 => SemanticOperator.Int8Op.Exp
            case Type.Int16 => SemanticOperator.Int16Op.Exp
            case Type.Int32 => SemanticOperator.Int32Op.Exp
            case Type.Int64 => SemanticOperator.Int64Op.Exp
            case Type.BigInt => SemanticOperator.BigIntOp.Exp
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Less => e1.tpe match {
            case Type.Char => SemanticOperator.CharOp.Lt
            case Type.Float32 => SemanticOperator.Float32Op.Lt
            case Type.Float64 => SemanticOperator.Float64Op.Lt
            case Type.Int8 => SemanticOperator.Int8Op.Lt
            case Type.Int16 => SemanticOperator.Int16Op.Lt
            case Type.Int32 => SemanticOperator.Int32Op.Lt
            case Type.Int64 => SemanticOperator.Int64Op.Lt
            case Type.BigInt => SemanticOperator.BigIntOp.Lt
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.LessEqual => e1.tpe match {
            case Type.Char => SemanticOperator.CharOp.Le
            case Type.Float32 => SemanticOperator.Float32Op.Le
            case Type.Float64 => SemanticOperator.Float64Op.Le
            case Type.Int8 => SemanticOperator.Int8Op.Le
            case Type.Int16 => SemanticOperator.Int16Op.Le
            case Type.Int32 => SemanticOperator.Int32Op.Le
            case Type.Int64 => SemanticOperator.Int64Op.Le
            case Type.BigInt => SemanticOperator.BigIntOp.Le
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Greater => e1.tpe match {
            case Type.Char => SemanticOperator.CharOp.Gt
            case Type.Float32 => SemanticOperator.Float32Op.Gt
            case Type.Float64 => SemanticOperator.Float64Op.Gt
            case Type.Int8 => SemanticOperator.Int8Op.Gt
            case Type.Int16 => SemanticOperator.Int16Op.Gt
            case Type.Int32 => SemanticOperator.Int32Op.Gt
            case Type.Int64 => SemanticOperator.Int64Op.Gt
            case Type.BigInt => SemanticOperator.BigIntOp.Gt
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.GreaterEqual => e1.tpe match {
            case Type.Char => SemanticOperator.CharOp.Ge
            case Type.Float32 => SemanticOperator.Float32Op.Ge
            case Type.Float64 => SemanticOperator.Float64Op.Ge
            case Type.Int8 => SemanticOperator.Int8Op.Ge
            case Type.Int16 => SemanticOperator.Int16Op.Ge
            case Type.Int32 => SemanticOperator.Int32Op.Ge
            case Type.Int64 => SemanticOperator.Int64Op.Ge
            case Type.BigInt => SemanticOperator.BigIntOp.Ge
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.Equal => e1.tpe match {
            case Type.Bool => SemanticOperator.BoolOp.Eq
            case Type.Char => SemanticOperator.CharOp.Eq
            case Type.Float32 => SemanticOperator.Float32Op.Eq
            case Type.Float64 => SemanticOperator.Float64Op.Eq
            case Type.Int8 => SemanticOperator.Int8Op.Eq
            case Type.Int16 => SemanticOperator.Int16Op.Eq
            case Type.Int32 => SemanticOperator.Int32Op.Eq
            case Type.Int64 => SemanticOperator.Int64Op.Eq
            case Type.BigInt => SemanticOperator.BigIntOp.Eq
            case Type.Str => SemanticOperator.StringOp.Eq
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.NotEqual => e1.tpe match {
            case Type.Bool => SemanticOperator.BoolOp.Neq
            case Type.Char => SemanticOperator.CharOp.Neq
            case Type.Float32 => SemanticOperator.Float32Op.Neq
            case Type.Float64 => SemanticOperator.Float64Op.Neq
            case Type.Int8 => SemanticOperator.Int8Op.Neq
            case Type.Int16 => SemanticOperator.Int16Op.Neq
            case Type.Int32 => SemanticOperator.Int32Op.Neq
            case Type.Int64 => SemanticOperator.Int64Op.Neq
            case Type.BigInt => SemanticOperator.BigIntOp.Neq
            case Type.Str => SemanticOperator.StringOp.Neq
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.LogicalAnd => e1.tpe match {
            case Type.Bool => SemanticOperator.BoolOp.And
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.LogicalOr => e1.tpe match {
            case Type.Bool => SemanticOperator.BoolOp.Or
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseAnd => e1.tpe match {
            case Type.Int8 => SemanticOperator.Int8Op.And
            case Type.Int16 => SemanticOperator.Int16Op.And
            case Type.Int32 => SemanticOperator.Int32Op.And
            case Type.Int64 => SemanticOperator.Int64Op.And
            case Type.BigInt => SemanticOperator.BigIntOp.And
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseOr => e1.tpe match {
            case Type.Int8 => SemanticOperator.Int8Op.Or
            case Type.Int16 => SemanticOperator.Int16Op.Or
            case Type.Int32 => SemanticOperator.Int32Op.Or
            case Type.Int64 => SemanticOperator.Int64Op.Or
            case Type.BigInt => SemanticOperator.BigIntOp.Or
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseXor => e1.tpe match {
            case Type.Int8 => SemanticOperator.Int8Op.Xor
            case Type.Int16 => SemanticOperator.Int16Op.Xor
            case Type.Int32 => SemanticOperator.Int32Op.Xor
            case Type.Int64 => SemanticOperator.Int64Op.Xor
            case Type.BigInt => SemanticOperator.BigIntOp.Xor
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseLeftShift => e1.tpe match {
            case Type.Int8 => SemanticOperator.Int8Op.Shl
            case Type.Int16 => SemanticOperator.Int16Op.Shl
            case Type.Int32 => SemanticOperator.Int32Op.Shl
            case Type.Int64 => SemanticOperator.Int64Op.Shl
            case Type.BigInt => SemanticOperator.BigIntOp.Shl
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
          case BinaryOperator.BitwiseRightShift => e1.tpe match {
            case Type.Int8 => SemanticOperator.Int8Op.Shr
            case Type.Int16 => SemanticOperator.Int16Op.Shr
            case Type.Int32 => SemanticOperator.Int32Op.Shr
            case Type.Int64 => SemanticOperator.Int64Op.Shr
            case Type.BigInt => SemanticOperator.BigIntOp.Shr
            case t => throw InternalCompilerException(s"Unexpected type: '$t' near ${loc.format}.")
          }
        }

        SimplifiedAst.Expression.Binary(sop, op, simplify(e1), simplify(e2), tpe, loc)

      case TypedAst.Expression.IfThenElse(e1, e2, e3, tpe, eff, loc) =>
        SimplifiedAst.Expression.IfThenElse(simplify(e1), simplify(e2), simplify(e3), tpe, loc)
      case TypedAst.Expression.Switch(rules, tpe, eff, loc) =>
        val zero = SimplifiedAst.Expression.SwitchError(tpe, loc)
        rules.foldRight(zero: SimplifiedAst.Expression) {
          case ((e1, e2), acc) =>
            val cond = simplify(e1)
            val body = simplify(e2)
            SimplifiedAst.Expression.IfThenElse(cond, body, acc, tpe, loc)
        }
      case TypedAst.Expression.Let(sym, e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expression.Let(sym, simplify(e1), simplify(e2), tpe, loc)

      case TypedAst.Expression.LetRec(sym, e1, e2, tpe, eff, loc) =>
        SimplifiedAst.Expression.LetRec(sym, simplify(e1), simplify(e2), tpe, loc)

      case TypedAst.Expression.Match(exp0, rules, tpe, eff, loc) =>
        import SimplifiedAst.{Expression => SExp}

        /**
          * Given the code:
          *
          * match x with {
          * case PATTERN_1 => BODY_1
          * case PATTERN_2 => BODY_2
          * ...
          * case PATTERN_N => BODY_N
          * }
          *
          * The structure of the generated code is as follows:
          *
          * let v' = x in
          * let fallthrough = fn () = ERROR in
          *
          * let v_n = fn () if (PATTERN_N succeeds) BODY_N else fallthrough() in
          * ...
          * let v_2 = fn () = if (PATTERN_2 succeeds) BODY_2 else v_3() in
          * let v_1 = fn () = if (PATTERN_1 succeeds) BODY_1 else v_2() in
          * v_1()
          */

        /**
          * First, we generate a temporary variable `matchVar` for the match expression `matchExp`
          * to avoid the match expression from being evaluated by every case test.
          *
          * The `matchVar` is used by every case in the pattern match to test the value.
          */
        val matchVar = Symbol.freshVarSym("matchVar")
        val matchExp = simplify(exp0)
        val fallthrough = Symbol.freshVarSym("case")

        /**
          * Second, we generate a fresh variable name for each case, as well as a fresh variable name for the
          * fallthrough case (which matches anything and throws a match error).
          */
        val vars = rules.map(_ => Symbol.freshVarSym("case"))
        val cases = rules

        /**
          * Third, we use recursion to generate the nested let-bindings for all the cases of the pattern match. To make
          * the recursion easier, the initial call reverses `names` and `cases`, since we're building the let-bindings
          * outside-in (from the last case to the first case).
          */
        def recur(names: List[Symbol.VarSym],
                  cases: List[TypedAst.MatchRule],
                  next: Symbol.VarSym): SExp = ((names, cases): @unchecked) match {
          case (Nil, Nil) =>
            // Base case: simply call the function representing the first case, to start the pattern match.
            SExp.Apply(SExp.Var(vars.head, Type.mkArrow(List(), tpe), loc), List(), tpe, loc)
          case (n :: ns, TypedAst.MatchRule(pat, guard, body) :: cs) =>
            // Construct the lambda that represents the current case:
            //   fn() = if `matchVar` matches `pat`, return `body`, else call `next()`
            val lambda = SExp.Lambda(
              args = List(),
              body = Patterns.simplify(
                xs = List(pat),
                ys = List(matchVar),
                guard,
                succ = simplify(body),
                fail = SExp.Apply(SExp.Var(next, Type.mkArrow(List(), tpe), loc), List(), tpe, loc)
              ),
              Type.mkArrow(List(), tpe), loc)

            // Construct the let-expression, binding the lambda to the current case's `name`.
            // Recursively construct the body of the let-expression, on the remaining names and cases.
            // In the recursive call, the `next` name is `n`, the name of the case we just processed.
            SExp.Let(n, lambda, recur(ns, cs, n), tpe, loc)
        }

        val patterns = recur(vars.reverse, cases.reverse, fallthrough)

        /**
          * Fourth, we generate the match error and bind it to the `fallthrough` name. Note that the match error must
          * be wrapped in a function call, to defer its evaluation.
          */
        val error = SExp.Lambda(List(), SExp.MatchError(tpe, loc), Type.mkArrow(List(), tpe), loc)
        val inner = SExp.Let(fallthrough, error, patterns, tpe, loc)

        /**
          * Finally, we generate the outermost let-binding, which binds the `matchExp` to `matchVar`.
          */
        SExp.Let(matchVar, matchExp, inner, tpe, loc)

      case TypedAst.Expression.Tag(sym, tag, e, tpe, eff, loc) =>
        SimplifiedAst.Expression.Tag(sym, tag, simplify(e), tpe, loc)

      case TypedAst.Expression.Tuple(elms, tpe, eff, loc) =>
        SimplifiedAst.Expression.Tuple(elms map simplify, tpe, loc)

      case TypedAst.Expression.Ref(exp, tpe, eff, loc) =>
        val e = simplify(exp)
        SimplifiedAst.Expression.Ref(e, tpe, loc)

      case TypedAst.Expression.Deref(exp, tpe, eff, loc) =>
        val e = simplify(exp)
        SimplifiedAst.Expression.Deref(e, tpe, loc)

      case TypedAst.Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        val e1 = simplify(exp1)
        val e2 = simplify(exp2)
        SimplifiedAst.Expression.Assign(e1, e2, tpe, loc)

      case TypedAst.Expression.Existential(fparam, exp, eff, loc) =>
        val p = SimplifiedAst.FormalParam(fparam.sym, fparam.mod, fparam.tpe, fparam.loc)
        val e = simplify(exp)
        SimplifiedAst.Expression.Existential(p, e, loc)

      case TypedAst.Expression.Universal(fparam, exp, eff, loc) =>
        val p = SimplifiedAst.FormalParam(fparam.sym, fparam.mod, fparam.tpe, fparam.loc)
        val e = simplify(exp)
        SimplifiedAst.Expression.Universal(p, e, loc)

      case TypedAst.Expression.Ascribe(exp, tpe, eff, loc) => simplify(exp)

      case TypedAst.Expression.Cast(exp, tpe, eff, loc) => simplify(exp)

      case TypedAst.Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
        val es = args.map(e => simplify(e))
        SimplifiedAst.Expression.NativeConstructor(constructor, es, tpe, loc)

      case TypedAst.Expression.NativeField(field, tpe, eff, loc) =>
        SimplifiedAst.Expression.NativeField(field, tpe, loc)

      case TypedAst.Expression.NativeMethod(method, args, tpe, eff, loc) =>
        val es = args.map(e => simplify(e))
        SimplifiedAst.Expression.NativeMethod(method, es, tpe, loc)

      case TypedAst.Expression.UserError(tpe, eff, loc) =>
        SimplifiedAst.Expression.UserError(tpe, loc)
    }
  }

  object Patterns {

    import SimplifiedAst.{Expression => SExp}
    import TypedAst.Pattern._

    // TODO: Have some one carefully peer-review this. esp. w.r.t. types.

    /**
      * Eliminates pattern matching.
      */
    def simplify(xs: List[TypedAst.Pattern],
                 ys: List[Symbol.VarSym],
                 guard: TypedAst.Expression,
                 succ: SExp,
                 fail: SExp)(implicit genSym: GenSym): SExp = ((xs, ys): @unchecked) match {
      /**
        * There are no more patterns and variables to match.
        *
        * The pattern was match successfully. Test the guard.
        */
      case (Nil, Nil) =>
        val g = Expression.simplify(guard)
        SExp.IfThenElse(g, succ, fail, succ.tpe, g.loc)

      /**
        * Matching a wildcard is guaranteed to succeed.
        *
        * We proceed by recursion on the remaining patterns and variables.
        */
      case (Wild(tpe, loc) :: ps, v :: vs) =>
        simplify(ps, vs, guard, succ, fail)

      /**
        * Matching a variable is guaranteed to succeed.
        *
        * We proceed by constructing a let-binding that binds the value
        * of the match variable `ident` to the variable `v`.
        * The body of the let-binding is computed by recursion on the
        * remaining patterns and variables.
        */
      case (Var(sym, tpe, loc) :: ps, v :: vs) =>
        val exp = simplify(ps, vs, guard, succ, fail)
        SExp.Let(sym, SExp.Var(v, tpe, loc), exp, succ.tpe, loc)

      /**
        * Matching a literal may succeed or fail.
        *
        * We generate a binary expression testing whether the literal `lit`
        * matches the variable `v` and then we generate an if-then-else
        * expression where the consequent expression is determined by
        * recursion on the remaining patterns and variables and the
        * alternative expression is `fail`.
        */
      case (lit :: ps, v :: vs) if isLiteral(lit) =>
        val exp = simplify(ps, vs, guard, succ, fail)
        val cond = mkEqual(pat2exp(lit), SExp.Var(v, lit.tpe, lit.loc), lit.loc)
        SExp.IfThenElse(cond, exp, fail, succ.tpe, lit.loc)

      /**
        * Matching a tag may succeed or fail.
        *
        * We generate a binary expression testing whether the tag name `tag`
        * matches the tag extracted from the variable `v` and then we generate
        * an if-then-else expression where the consequent expression is determined
        * by recursion on the remaining patterns and variables together with the
        * nested pattern of the tag added in front and a new fresh variable holding
        * the value of the tag.
        */
      case (Tag(sym, tag, pat, tpe, loc) :: ps, v :: vs) =>
        val cond = SExp.Is(sym, tag, SExp.Var(v, tpe, loc), loc)
        val freshVar = Symbol.freshVarSym("innerTag")
        val inner = simplify(pat :: ps, freshVar :: vs, guard, succ, fail)
        val consequent = SExp.Let(freshVar, SExp.Untag(sym, tag, SExp.Var(v, tpe, loc), pat.tpe, loc), inner, succ.tpe, loc)
        SExp.IfThenElse(cond, consequent, fail, succ.tpe, loc)

      /**
        * Matching a tuple may succeed or fail.
        *
        * We generate a fresh variable and let-binding for each component of the
        * tuple and then we recurse on the nested patterns and freshly generated
        * variables.
        */
      case (Tuple(elms, tpe, loc) :: ps, v :: vs) =>
        val freshVars = elms.map(_ => Symbol.freshVarSym("innerElm"))
        val zero = simplify(elms ::: ps, freshVars ::: vs, guard, succ, fail)
        elms.zip(freshVars).zipWithIndex.foldRight(zero) {
          case (((pat, name), idx), exp) =>
            SExp.Let(name, SExp.Index(SExp.Var(v, tpe, loc), idx, pat.tpe, loc), exp, succ.tpe, loc)
        }

      case p => throw InternalCompilerException(s"Unsupported pattern '$p'.")

    }

    /**
      * Translates the given typed pattern `pat0` into a simplified pattern.
      *
      * NB: This function is only used for constraints. Patterns are eliminated for expressions.
      */
    def translate(pat0: TypedAst.Pattern): SimplifiedAst.Pattern = pat0 match {
      case TypedAst.Pattern.Wild(tpe, loc) => SimplifiedAst.Pattern.Wild(tpe, loc)
      case TypedAst.Pattern.Var(sym, tpe, loc) => SimplifiedAst.Pattern.Var(sym, tpe, loc)
      case TypedAst.Pattern.Unit(loc) => SimplifiedAst.Pattern.Unit(loc)
      case TypedAst.Pattern.True(loc) => SimplifiedAst.Pattern.True(loc)
      case TypedAst.Pattern.False(loc) => SimplifiedAst.Pattern.False(loc)
      case TypedAst.Pattern.Char(lit, loc) => SimplifiedAst.Pattern.Char(lit, loc)
      case TypedAst.Pattern.Float32(lit, loc) => SimplifiedAst.Pattern.Float32(lit, loc)
      case TypedAst.Pattern.Float64(lit, loc) => SimplifiedAst.Pattern.Float64(lit, loc)
      case TypedAst.Pattern.Int8(lit, loc) => SimplifiedAst.Pattern.Int8(lit, loc)
      case TypedAst.Pattern.Int16(lit, loc) => SimplifiedAst.Pattern.Int16(lit, loc)
      case TypedAst.Pattern.Int32(lit, loc) => SimplifiedAst.Pattern.Int32(lit, loc)
      case TypedAst.Pattern.Int64(lit, loc) => SimplifiedAst.Pattern.Int64(lit, loc)
      case TypedAst.Pattern.BigInt(lit, loc) => SimplifiedAst.Pattern.BigInt(lit, loc)
      case TypedAst.Pattern.Str(lit, loc) => SimplifiedAst.Pattern.Str(lit, loc)
      case TypedAst.Pattern.Tag(sym, tag, pat, tpe, loc) => SimplifiedAst.Pattern.Tag(sym, tag, translate(pat), tpe, loc)
      case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
        val es = elms map translate
        SimplifiedAst.Pattern.Tuple(es, tpe, loc)
    }

    /**
      * Returns an expression that compares the two given expressions `e1` and `e2` for equality.
      */
    private def mkEqual(e1: SimplifiedAst.Expression, e2: SimplifiedAst.Expression, loc: SourceLocation): SimplifiedAst.Expression = {
      /*
       * Special Case 1: Unit
       */
      (e1.tpe, e2.tpe) match {
        case (Type.Unit, Type.Unit) =>
          // Unit is always equal to itself.
          return SimplifiedAst.Expression.True
        case _ => // fallthrough
      }

      /*
       * Compute the semantic operator.
       */
      val sop = e1.tpe match {
        case Type.Bool => SemanticOperator.BoolOp.Eq
        case Type.Char => SemanticOperator.CharOp.Eq
        case Type.Float32 => SemanticOperator.Float32Op.Eq
        case Type.Float64 => SemanticOperator.Float64Op.Eq
        case Type.Int8 => SemanticOperator.Int8Op.Eq
        case Type.Int16 => SemanticOperator.Int16Op.Eq
        case Type.Int32 => SemanticOperator.Int32Op.Eq
        case Type.Int64 => SemanticOperator.Int64Op.Eq
        case Type.BigInt => SemanticOperator.BigIntOp.Eq
        case Type.Str => SemanticOperator.StringOp.Eq
        case t => throw InternalCompilerException(s"Unexpected type: '$t'.")
      }

      SExp.Binary(sop, BinaryOperator.Equal, e1, e2, Type.Bool, loc)
    }

  }

  object Predicate {

    object Head {
      def simplify(tast: TypedAst.Predicate.Head, cparams: List[TypedAst.ConstraintParam], toplevel: TopLevel)(implicit genSym: GenSym): SimplifiedAst.Predicate.Head = tast match {
        case TypedAst.Predicate.Head.True(loc) => SimplifiedAst.Predicate.Head.True(loc)

        case TypedAst.Predicate.Head.False(loc) => SimplifiedAst.Predicate.Head.False(loc)

        case TypedAst.Predicate.Head.Positive(sym, terms, loc) =>
          val ts = terms.map(t => Term.Head.simplify(t, cparams, toplevel))
          SimplifiedAst.Predicate.Head.Positive(sym, ts, loc)

        case TypedAst.Predicate.Head.Negative(sym, terms, loc) =>
          val ts = terms.map(t => Term.Head.simplify(t, cparams, toplevel))
          SimplifiedAst.Predicate.Head.Negative(sym, ts, loc)
      }
    }

    object Body {
      def simplify(tast: TypedAst.Predicate.Body, cparams: List[TypedAst.ConstraintParam], toplevel: TopLevel)(implicit genSym: GenSym): SimplifiedAst.Predicate.Body = tast match {
        case TypedAst.Predicate.Body.Positive(sym, terms, loc) =>
          val ts = terms map Term.Body.simplify
          SimplifiedAst.Predicate.Body.Positive(sym, ts, loc)

        case TypedAst.Predicate.Body.Negative(sym, terms, loc) =>
          val ts = terms map Term.Body.simplify
          SimplifiedAst.Predicate.Body.Negative(sym, ts, loc)

        case TypedAst.Predicate.Body.Filter(sym, terms, loc) =>
          SimplifiedAst.Predicate.Body.Filter(sym, terms map Term.Body.simplify, loc)

        case TypedAst.Predicate.Body.Loop(sym, term, loc) =>
          val cps = cparams.filter {
            case TypedAst.ConstraintParam.HeadParam(sym2, _, _) => sym != sym2
            case TypedAst.ConstraintParam.RuleParam(sym2, _, _) => sym != sym2
          }
          SimplifiedAst.Predicate.Body.Loop(sym, Term.Head.simplify(term, cps, toplevel), loc)
      }
    }

  }

  object Term {

    object Head {
      /**
        * Simplifies the given head term expression `e0`.
        */
      def simplify(e0: TypedAst.Expression, cparams: List[TypedAst.ConstraintParam], toplevel: TopLevel)(implicit genSym: GenSym): SimplifiedAst.Term.Head = e0 match {
        case TypedAst.Expression.Var(sym, tpe, eff, loc) =>
          SimplifiedAst.Term.Head.Var(sym, tpe, loc)

        case TypedAst.Expression.Apply(TypedAst.Expression.Def(sym, _, _, _), args, tpe, eff, loc) if isVarExps(args) =>
          val as = args map {
            case TypedAst.Expression.Var(x, _, _, _) => x
            case e => throw InternalCompilerException(s"Unexpected non-variable expression: $e.")
          }
          SimplifiedAst.Term.Head.App(sym, as, tpe, loc)

        case _ =>
          // Determine if the expression is a literal.
          if (isLiteral(e0)) {
            // The expression is a literal.
            val lit = Expression.simplify(e0)
            SimplifiedAst.Term.Head.Lit(lit, e0.tpe, e0.loc)
          } else {
            // The expression is not a literal.
            // Must create a new top-level definition for the expression.

            // Generate a fresh symbol for the new definition.
            val freshSym = Symbol.freshDefnSym("head")

            // Generate fresh symbols for the formal parameters of the definition.
            val freshSymbols = cparams.map {
              case TypedAst.ConstraintParam.HeadParam(sym, tpe, _) => sym -> (Symbol.freshVarSym(sym), tpe)
              case TypedAst.ConstraintParam.RuleParam(sym, tpe, _) => sym -> (Symbol.freshVarSym(sym), tpe)
            }

            // Generate fresh formal parameters for the definition.
            val formals = freshSymbols.map {
              case ((oldSym, (newSym, tpe))) => SimplifiedAst.FormalParam(newSym, Ast.Modifiers.Empty, tpe, oldSym.loc)
            }

            // The expression of the fresh definition is simply `e0` with fresh local variables.
            val exp = copy(Expression.simplify(e0), freshSymbols.map(x => (x._1, x._2._1)).toMap)

            // Construct the definition type.
            val arrowType = Type.mkArrow(freshSymbols.map(_._2._2), e0.tpe)

            // Assemble the fresh definition.
            val defn = SimplifiedAst.Def(Ast.Annotations(Nil), Ast.Modifiers.Empty, freshSym, formals, exp, isSynthetic = true, arrowType, e0.loc)

            // Add the fresh definition to the top-level.
            toplevel += freshSym -> defn

            // Compute the argument symbols, i.e. the symbols of the rule.
            val argSymbols = freshSymbols.map(_._1)

            // Return a head term that calls the freshly generated top-level definition.
            SimplifiedAst.Term.Head.App(freshSym, argSymbols, e0.tpe, e0.loc)
          }
      }

      /**
        * Returns `true` if all the given expressions `exps` are variable expressions.
        */
      private def isVarExps(args: List[TypedAst.Expression]): Boolean = args.forall(_.isInstanceOf[TypedAst.Expression.Var])
    }


    object Body {
      /**
        * Simplifies the given body term pattern `p`.
        */
      def simplify(p: TypedAst.Pattern)(implicit genSym: GenSym): SimplifiedAst.Term.Body = p match {
        case TypedAst.Pattern.Wild(tpe, loc) => SimplifiedAst.Term.Body.Wild(tpe, loc)
        case TypedAst.Pattern.Var(sym, tpe, loc) => SimplifiedAst.Term.Body.Var(sym, tpe, loc)
        case _ => if (isLiteral(p))
          SimplifiedAst.Term.Body.Lit(pat2exp(p), p.tpe, p.loc)
        else
          SimplifiedAst.Term.Body.Pat(Patterns.translate(p), p.tpe, p.loc)
      }

      /**
        * Simplifies the given body term expression `e`.
        */
      def simplify(e: TypedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Term.Body = e match {
        case TypedAst.Expression.Wild(tpe, eff, loc) => SimplifiedAst.Term.Body.Wild(tpe, loc)
        case TypedAst.Expression.Var(sym, tpe, eff, loc) => SimplifiedAst.Term.Body.Var(sym, tpe, loc)
        case _ => SimplifiedAst.Term.Body.Lit(Expression.simplify(e), e.tpe, e.loc) // TODO: Only certain expressions should be allow here.
      }
    }

  }

  def simplify(a: TypedAst.Attribute)(implicit genSym: GenSym): SimplifiedAst.Attribute =
    SimplifiedAst.Attribute(a.name, a.tpe)

  def simplify(p: TypedAst.FormalParam)(implicit genSym: GenSym): SimplifiedAst.FormalParam =
    SimplifiedAst.FormalParam(p.sym, p.mod, p.tpe, p.loc)

  def simplify(p: TypedAst.Property)(implicit genSym: GenSym): SimplifiedAst.Property =
    SimplifiedAst.Property(p.law, p.defn, Expression.simplify(p.exp))

  /**
    * Returns `true` if the given pattern `pat0` is a literal.
    */
  def isLiteral(pat0: TypedAst.Pattern): Boolean = pat0 match {
    case TypedAst.Pattern.Unit(loc) => true
    case TypedAst.Pattern.True(loc) => true
    case TypedAst.Pattern.False(loc) => true
    case TypedAst.Pattern.Char(lit, loc) => true
    case TypedAst.Pattern.Float32(lit, loc) => true
    case TypedAst.Pattern.Float64(lit, loc) => true
    case TypedAst.Pattern.Int8(lit, loc) => true
    case TypedAst.Pattern.Int16(lit, loc) => true
    case TypedAst.Pattern.Int32(lit, loc) => true
    case TypedAst.Pattern.Int64(lit, loc) => true
    case TypedAst.Pattern.BigInt(lit, loc) => true
    case TypedAst.Pattern.Str(lit, loc) => true
    case _ => false
  }

  /**
    * Returns `true` if the given expression `exp0` is a literal.
    */
  def isLiteral(exp0: TypedAst.Expression): Boolean = exp0 match {
    case TypedAst.Expression.Unit(loc) => true
    case TypedAst.Expression.True(loc) => true
    case TypedAst.Expression.False(loc) => true
    case TypedAst.Expression.Char(lit, loc) => true
    case TypedAst.Expression.Float32(lit, loc) => true
    case TypedAst.Expression.Float64(lit, loc) => true
    case TypedAst.Expression.Int8(lit, loc) => true
    case TypedAst.Expression.Int16(lit, loc) => true
    case TypedAst.Expression.Int32(lit, loc) => true
    case TypedAst.Expression.Int64(lit, loc) => true
    case TypedAst.Expression.BigInt(lit, loc) => true
    case TypedAst.Expression.Str(lit, loc) => true
    case TypedAst.Expression.Tag(sym, tag, exp, tpe, eff, loc) => isLiteral(exp)
    case TypedAst.Expression.Tuple(elms, tpe, eff, loc) => elms forall isLiteral
    case _ => false
  }

  /**
    * Returns the given pattern `pat0` as an expression.
    */
  def pat2exp(pat0: TypedAst.Pattern): SimplifiedAst.Expression = pat0 match {
    case TypedAst.Pattern.Unit(loc) => SimplifiedAst.Expression.Unit
    case TypedAst.Pattern.True(loc) => SimplifiedAst.Expression.True
    case TypedAst.Pattern.False(loc) => SimplifiedAst.Expression.False
    case TypedAst.Pattern.Char(lit, loc) => SimplifiedAst.Expression.Char(lit)
    case TypedAst.Pattern.Float32(lit, loc) => SimplifiedAst.Expression.Float32(lit)
    case TypedAst.Pattern.Float64(lit, loc) => SimplifiedAst.Expression.Float64(lit)
    case TypedAst.Pattern.Int8(lit, loc) => SimplifiedAst.Expression.Int8(lit)
    case TypedAst.Pattern.Int16(lit, loc) => SimplifiedAst.Expression.Int16(lit)
    case TypedAst.Pattern.Int32(lit, loc) => SimplifiedAst.Expression.Int32(lit)
    case TypedAst.Pattern.Int64(lit, loc) => SimplifiedAst.Expression.Int64(lit)
    case TypedAst.Pattern.BigInt(lit, loc) => SimplifiedAst.Expression.BigInt(lit)
    case TypedAst.Pattern.Str(lit, loc) => SimplifiedAst.Expression.Str(lit)
    case TypedAst.Pattern.Tag(sym, tag, p, tpe, loc) => SimplifiedAst.Expression.Tag(sym, tag, pat2exp(p), tpe, loc)
    case TypedAst.Pattern.Tuple(elms, tpe, loc) => SimplifiedAst.Expression.Tuple(elms map pat2exp, tpe, loc)
    case _ => throw InternalCompilerException(s"Unexpected non-literal pattern $pat0.")
  }

  /**
    * Returns a copy of the given expression `exp0` where every variable symbol
    * has been replaced according to the given substitution `m`.
    */
  private def copy(exp0: SimplifiedAst.Expression, m: Map[Symbol.VarSym, Symbol.VarSym]): SimplifiedAst.Expression = {
    def visit(e: SimplifiedAst.Expression): SimplifiedAst.Expression = e match {
      case SimplifiedAst.Expression.Unit => e
      case SimplifiedAst.Expression.True => e
      case SimplifiedAst.Expression.False => e
      case SimplifiedAst.Expression.Char(lit) => e
      case SimplifiedAst.Expression.Float32(lit) => e
      case SimplifiedAst.Expression.Float64(lit) => e
      case SimplifiedAst.Expression.Int8(lit) => e
      case SimplifiedAst.Expression.Int16(lit) => e
      case SimplifiedAst.Expression.Int32(lit) => e
      case SimplifiedAst.Expression.Int64(lit) => e
      case SimplifiedAst.Expression.BigInt(lit) => e
      case SimplifiedAst.Expression.Str(lit) => e
      case SimplifiedAst.Expression.Var(sym, tpe, loc) => m.get(sym) match {
        case None => SimplifiedAst.Expression.Var(sym, tpe, loc)
        case Some(replacement) => SimplifiedAst.Expression.Var(replacement, tpe, loc)
      }
      case SimplifiedAst.Expression.Def(name, tpe, loc) => e
      case SimplifiedAst.Expression.Lambda(fparams, body, tpe, loc) =>
        SimplifiedAst.Expression.Lambda(fparams, visit(body), tpe, loc)
      case SimplifiedAst.Expression.Hook(hook, tpe, loc) => e
      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
        SimplifiedAst.Expression.Apply(visit(exp), args.map(visit), tpe, loc)
      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        SimplifiedAst.Expression.Unary(sop, op, visit(exp), tpe, loc)
      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Binary(sop, op, visit(exp1), visit(exp2), tpe, loc)
      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        SimplifiedAst.Expression.IfThenElse(visit(exp1), visit(exp2), visit(exp3), tpe, loc)
      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> br
        }
        SimplifiedAst.Expression.Branch(e, bs, tpe, loc)
      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        SimplifiedAst.Expression.JumpTo(sym, tpe, loc)
      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Let(sym, visit(exp1), visit(exp2), tpe, loc)
      case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.LetRec(sym, visit(exp1), visit(exp2), tpe, loc)
      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        SimplifiedAst.Expression.Is(sym, tag, visit(exp), loc)
      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        SimplifiedAst.Expression.Tag(enum, tag, visit(exp), tpe, loc)
      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        SimplifiedAst.Expression.Untag(sym, tag, visit(exp), tpe, loc)
      case SimplifiedAst.Expression.Index(exp, offset, tpe, loc) =>
        SimplifiedAst.Expression.Index(visit(exp), offset, tpe, loc)
      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        SimplifiedAst.Expression.Tuple(elms.map(visit), tpe, loc)
      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        SimplifiedAst.Expression.Ref(visit(exp), tpe, loc)
      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        SimplifiedAst.Expression.Deref(visit(exp), tpe, loc)
      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        SimplifiedAst.Expression.Assign(visit(exp1), visit(exp2), tpe, loc)
      case SimplifiedAst.Expression.Existential(params, exp, loc) =>
        SimplifiedAst.Expression.Existential(params, visit(exp), loc)
      case SimplifiedAst.Expression.Universal(params, exp, loc) =>
        SimplifiedAst.Expression.Universal(params, visit(exp), loc)
      case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args map visit
        SimplifiedAst.Expression.NativeConstructor(constructor, es, tpe, loc)
      case SimplifiedAst.Expression.NativeField(field, tpe, loc) =>
        SimplifiedAst.Expression.NativeField(field, tpe, loc)
      case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args map visit
        SimplifiedAst.Expression.NativeMethod(method, es, tpe, loc)
      case SimplifiedAst.Expression.UserError(tpe, loc) => e
      case SimplifiedAst.Expression.MatchError(tpe, loc) => e
      case SimplifiedAst.Expression.SwitchError(tpe, loc) => e

      case SimplifiedAst.Expression.Closure(ref, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyDef(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplySelfTail(name, formals, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case SimplifiedAst.Expression.ApplyHook(hook, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    }

    visit(exp0)
  }

}
