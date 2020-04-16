/*
 *  Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Scheme.InstantiateMode
import ca.uwaterloo.flix.language.ast.{Rigidity, Scheme, SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.UnificationError
import ca.uwaterloo.flix.util.Result._
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec

object Unification {


  /**
    * Aliases to make the success variable elimination easier to understand.
    */
  private val True: Type = Type.Pure
  private val False: Type = Type.Impure

  /**
    * Companion object for the [[Substitution]] class.
    */
  object Substitution {
    /**
      * Returns the empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)

    /**
      * Returns the singleton substitution mapping the type variable `x` to `tpe`.
      */
    def singleton(x: Type.Var, tpe: Type): Substitution = {
      // Ensure that we do not add any x -> x mappings.
      tpe match {
        case y: Type.Var if x.id == y.id => empty
        case _ => Substitution(Map(x -> tpe))
      }
    }

  }

  /**
    * A substitution is a map from type variables to types.
    */
  case class Substitution(m: Map[Type.Var, Type]) {

    /**
      * Returns `true` if `this` is the empty substitution.
      */
    val isEmpty: Boolean = m.isEmpty

    /**
      * Applies `this` substitution to the given type `tpe0`.
      */
    def apply(tpe0: Type): Type = {
      def visit(t: Type): Type =
        t match {
          case x: Type.Var =>
            m.get(x) match {
              case None => x
              case Some(y) if x.kind == t.kind => y
              case Some(y) if x.kind != t.kind => throw InternalCompilerException(s"Expected kind `${x.kind}' but got `${t.kind}'.")
            }
          case Type.Cst(tc) => Type.Cst(tc)
          case Type.Arrow(l, eff) => Type.Arrow(l, visit(eff))
          case Type.RecordEmpty => Type.RecordEmpty
          case Type.RecordExtend(label, field, rest) => Type.RecordExtend(label, visit(field), visit(rest))
          case Type.SchemaEmpty => Type.SchemaEmpty
          case Type.SchemaExtend(sym, tpe, rest) => Type.SchemaExtend(sym, visit(tpe), visit(rest))
          case Type.Zero => Type.Zero
          case Type.Succ(n, t) => Type.Succ(n, visit(t))
          case Type.Apply(t1, t2) =>
            (visit(t1), visit(t2)) match {
              // Simplify boolean equations.
              case (Type.Cst(TypeConstructor.Not), x) => mkNot(x)
              case (Type.Apply(Type.Cst(TypeConstructor.And), x), y) => mkAnd(x, y)
              case (Type.Apply(Type.Cst(TypeConstructor.Or), x), y) => mkOr(x, y)
              case (x, y) => Type.Apply(x, y)
            }
          case Type.Lambda(tvar, tpe) => throw InternalCompilerException(s"Unexpected type '$tpe0'.")
        }

      // Optimization: Return the type if the substitution is empty. Otherwise visit the type.
      if (isEmpty) tpe0 else visit(tpe0)
    }

    /**
      * Applies `this` substitution to the given types `ts`.
      */
    def apply(ts: List[Type]): List[Type] = if (isEmpty) ts else ts map apply

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
        val newTypeMap = that.m.foldLeft(Map.empty[Type.Var, Type]) {
          case (macc, (x, t)) => macc.updated(x, this.apply(t))
        }
        Substitution(newTypeMap) ++ this
      }
    }

  }

  /**
    * A type inference state monad that maintains the current substitution.
    */
  case class InferMonad[A](run: Substitution => Result[(Substitution, A), TypeError]) {
    /**
      * Applies the given function `f` to the value in the monad.
      */
    def map[B](f: A => B): InferMonad[B] = {
      def runNext(s0: Substitution): Result[(Substitution, B), TypeError] = {
        // Run the original function and map over its result (since it may have error'd).
        run(s0) map {
          case (s, a) => (s, f(a))
        }
      }

      InferMonad(runNext)
    }

    /**
      * Applies the given function `f` to the value in the monad.
      */
    def flatMap[B](f: A => InferMonad[B]): InferMonad[B] = {
      def runNext(s0: Substitution): Result[(Substitution, B), TypeError] = {
        // Run the original function and flatMap over its result (since it may have error'd).
        run(s0) flatMap {
          case (s, a) => f(a) match {
            // Unwrap the returned monad and apply the inner function g.
            case InferMonad(g) => g(s)
          }
        }
      }

      InferMonad(runNext)
    }

    // TODO: Necessary for pattern matching?
    // TODO: What should this return?
    def withFilter(f: A => Boolean): InferMonad[A] = InferMonad(x => run(x) match {
      case Ok((subst, t)) => if (f(t)) Ok((subst, t)) else Ok((subst, t))
      case Err(e) => Err(e)
    })

  }


  /**
    * Returns the most general unifier of the two given types `tpe1` and `tpe2`.
    */
  def unifyTypes(tpe1: Type, tpe2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {

    /**
      * Unifies the given variable `x` with the given non-variable type `tpe`.
      */
    def unifyVar(x: Type.Var, tpe: Type): Result[Substitution, UnificationError] = {
      // NB: The `tpe` type must be a non-var.
      if (tpe.isInstanceOf[Type.Var])
        throw InternalCompilerException(s"Unexpected variable type: '$tpe'.")

      // Check if `x` is rigid.
      if (x.rigidity == Rigidity.Rigid) {
        return Result.Err(UnificationError.RigidVar(x, tpe))
      }

      // Check if `x` occurs within `tpe`.
      if (tpe.typeVars contains x) {
        return Result.Err(UnificationError.OccursCheck(x, tpe))
      }

      // Check if the kind of `x` matches the kind of `tpe`.

      //if (x.kind != tpe.kind) {
      //  return Result.Err(TypeError.KindError())
      //}

      // We can substitute `x` for `tpe`. Update the textual name of `tpe`.
      if (x.getText.nonEmpty && tpe.isInstanceOf[Type.Var]) {
        // TODO: Get rid of this insanity.
        tpe.asInstanceOf[Type.Var].setText(x.getText.get)
      }
      Result.Ok(Substitution.singleton(x, tpe))
    }

    /**
      * Unifies the two given types `tpe1` and `tpe2`.
      */
    // NB: The order of cases has been determined by code coverage analysis.
    def unifyTypes(tpe1: Type, tpe2: Type): Result[Substitution, UnificationError] = (tpe1, tpe2) match {
      case (x: Type.Var, y: Type.Var) =>
        // Case 1: Check if the type variables are syntactically the same.
        if (x.id == y.id && x.kind == y.kind)
          return Result.Ok(Substitution.empty)
        // Case 2: The left type variable is flexible.
        if (x.rigidity == Rigidity.Flexible)
          return Result.Ok(Substitution.singleton(x, y))
        // Case 3: The right type variable is flexible.
        if (y.rigidity == Rigidity.Flexible)
          return Result.Ok(Substitution.singleton(y, x))
        // Case 4: Both type variables are rigid.
        Result.Err(UnificationError.RigidVar(x, y))

      case (x: Type.Var, _) => unifyVar(x, tpe2)

      case (_, x: Type.Var) => unifyVar(x, tpe1)

      case (Type.Cst(c1), Type.Cst(c2)) if c1 == c2 => Result.Ok(Substitution.empty)

      case (Type.Apply(t11, t12), Type.Apply(t21, t22)) =>
        unifyTypes(t11, t21) match {
          case Result.Ok(subst1) => unifyTypes(subst1(t12), subst1(t22)) match {
            case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
            case Result.Err(e) => Result.Err(e)
          }
          case Result.Err(e) => Result.Err(e)
        }

      case (Type.Arrow(l1, eff1), Type.Arrow(l2, eff2)) if l1 == l2 => unifyEffects(eff1, eff2)

      case (Type.RecordEmpty, Type.RecordEmpty) => Result.Ok(Substitution.empty)

      case (Type.SchemaEmpty, Type.SchemaEmpty) => Result.Ok(Substitution.empty)

      case (Type.RecordExtend(label1, fieldType1, restRow1), row2) =>
        // Attempt to write the row to match.
        rewriteRow(row2, label1, fieldType1, row2) flatMap {
          case (subst1, restRow2) =>
            // TODO: Missing the safety/occurs check.
            unifyTypes(subst1(restRow1), subst1(restRow2)) flatMap {
              case subst2 => Result.Ok(subst2 @@ subst1)
            }
        }

      case (Type.SchemaExtend(sym, tpe, restRow1), row2) =>
        // Attempt to write the row to match.
        rewriteSchemaRow(row2, sym, tpe, row2) flatMap {
          case (subst1, restRow2) =>
            // TODO: Missing the safety/occurs check.
            unifyTypes(subst1(restRow1), subst1(restRow2)) flatMap {
              case subst2 => Result.Ok(subst2 @@ subst1)
            }
        }

      case (Type.Zero, Type.Zero) => Result.Ok(Substitution.empty) // 0 == 0

      case (Type.Succ(0, Type.Zero), Type.Zero) => Result.Ok(Substitution.empty)

      case (Type.Zero, Type.Succ(0, Type.Zero)) => Result.Ok(Substitution.empty)

      case (Type.Succ(n1, t1), Type.Succ(n2, t2)) if n1 == n2 => unifyTypes(t1, t2) //(42, t1) == (42, t2)

      case (Type.Succ(n1, t1), Type.Succ(n2, t2)) if n1 > n2 => unifyTypes(Type.Succ(n1 - n2, t1), t2) // (42, x) == (21 y) --> (42-21, x) = y

      case (Type.Succ(n1, t1), Type.Succ(n2, t2)) if n1 < n2 => unifyTypes(Type.Succ(n2 - n1, t2), t1) // (21, x) == (42, y) --> (42-21, y) = x

      case _ => Result.Err(UnificationError.MismatchedTypes(tpe1, tpe2))
    }

    /**
      * Unifies the two given lists of types `ts1` and `ts2`.
      */
    def unifyAll(ts1: List[Type], ts2: List[Type]): Result[Substitution, UnificationError] = {
      def visit(x: List[Type], y: List[Type]): Result[Substitution, UnificationError] = (x, y) match {
        case (Nil, Nil) => Result.Ok(Substitution.empty)
        case (t1 :: rs1, t2 :: rs2) => unifyTypes(t1, t2) match {
          case Result.Ok(subst1) => visit(subst1(rs1), subst1(rs2)) match {
            case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
            case Result.Err(e) => Result.Err(e)
          }
          case Result.Err(e) => Result.Err(e)
        }
        case _ => Result.Err(UnificationError.MismatchedArity(ts1, ts2))
      }

      visit(ts1, ts2)
    }

    /**
      * Attempts to rewrite the given row type `row2` into a row that has the given label `label1` in front.
      */
    def rewriteRow(row2: Type, label1: String, fieldType1: Type, originalType: Type): Result[(Substitution, Type), UnificationError] = row2 match {
      case Type.RecordExtend(label2, fieldType2, restRow2) =>
        // Case 1: The row is of the form %{ label2: fieldType2 | restRow2 }
        if (label1 == label2) {
          // Case 1.1: The labels match, their types must match.
          for {
            subst <- unifyTypes(fieldType1, fieldType2)
          } yield (subst, restRow2)
        } else {
          // Case 1.2: The labels do not match, attempt to match with a label further down.
          rewriteRow(restRow2, label1, fieldType1, originalType) map {
            case (subst, rewrittenRow) => (subst, Type.RecordExtend(label2, fieldType2, rewrittenRow))
          }
        }
      case tvar: Type.Var =>
        // Case 2: The row is a type variable.
        // Introduce a fresh type variable to represent one more level of the row.
        val restRow2 = Type.freshTypeVar()
        val type2 = Type.RecordExtend(label1, fieldType1, restRow2)
        val subst = Unification.Substitution.singleton(tvar, type2)
        Ok((subst, restRow2))

      case Type.RecordEmpty =>
        // Case 3: The `label` does not exist in the record.
        Err(UnificationError.UndefinedLabel(label1, fieldType1, originalType))

      case _ =>
        // Case 4: The type is not a row.
        Err(UnificationError.NonRecordType(row2))
    }

    /**
      * Attempts to rewrite the given row type `row2` into a row that has the given label `label1` in front.
      */
    // TODO: This is a copy of the above function. It would be nice if it could be the same function, but the shape of labels is different.
    def rewriteSchemaRow(row2: Type, label1: String, fieldType1: Type, originalType: Type): Result[(Substitution, Type), UnificationError] = row2 match {
      case Type.SchemaExtend(label2, fieldType2, restRow2) =>
        // Case 1: The row is of the form %{ label2: fieldType2 | restRow2 }
        if (label1 == label2) {
          // Case 1.1: The labels match, their types must match.
          for {
            subst <- unifyTypes(fieldType1, fieldType2)
          } yield (subst, restRow2)
        } else {
          // Case 1.2: The labels do not match, attempt to match with a label further down.
          rewriteSchemaRow(restRow2, label1, fieldType1, originalType) map {
            case (subst, rewrittenRow) => (subst, Type.SchemaExtend(label2, fieldType2, rewrittenRow))
          }
        }
      case tvar: Type.Var =>
        // Case 2: The row is a type variable.
        // Introduce a fresh type variable to represent one more level of the row.
        val restRow2 = Type.freshTypeVar()
        val type2 = Type.SchemaExtend(label1, fieldType1, restRow2)
        val subst = Unification.Substitution.singleton(tvar, type2)
        Ok((subst, restRow2))

      case Type.SchemaEmpty =>
        // Case 3: The `label` does not exist in the record.
        Err(UnificationError.UndefinedPredicate(label1, fieldType1, originalType))

      case _ =>
        // Case 4: The type is not a row.
        Err(UnificationError.NonSchemaType(row2))
    }

    unifyTypes(tpe1, tpe2)
  }

  /**
    * Returns the most general unifier of the two given effects `eff1` and `eff2`.
    */
  def unifyEffects(eff1: Type, eff2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // Determine if effect checking is enabled.
    if (flix.options.xnoeffects)
      return Ok(Substitution.empty)

    // The boolean expression we want to show is 0.
    val query = mkEq(eff1, eff2)

    // The free and flexible type (effect) variables in the query.
    val freeVars = query.typeVars.toList.filter(_.rigidity == Rigidity.Flexible)

    // Eliminate all variables.
    try {
      val subst = successiveVariableElimination(query, freeVars)

      // TODO: Debugging
      //    if (!subst.isEmpty) {
      //      val s = subst.toString
      //      val len = s.length
      //      if (len > 50) {
      //        println(s.substring(0, Math.min(len, 300)))
      //        println()
      //      }
      //    }

      Ok(subst)
    } catch {
      case BooleanUnificationException => Err(UnificationError.MismatchedEffects(eff1, eff2))
    }
  }

  /**
    * To unify two effects p and q it suffices to unify t = (p ∧ ¬q) ∨ (¬p ∧ q) and check t = 0.
    */
  private def mkEq(p: Type, q: Type): Type = mkOr(mkAnd(p, mkNot(q)), mkAnd(mkNot(p), q))

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    */
  private def successiveVariableElimination(f: Type, fvs: List[Type.Var])(implicit flix: Flix): Substitution = fvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      val q = Scheme.instantiate(Scheme(f.typeVars.toList, f), InstantiateMode.Flexible)
      if (!sat(q))
        Substitution.empty
      else
        throw BooleanUnificationException

    case x :: xs =>
      val t0 = Substitution.singleton(x, False)(f)
      val t1 = Substitution.singleton(x, True)(f)
      val se = successiveVariableElimination(mkAnd(t0, t1), xs)
      val st = Substitution.singleton(x, mkOr(se(t0), mkAnd(Type.freshTypeVar(), mkNot(se(t1)))))
      st ++ se
  }

  /**
    * An exception thrown to indicate that boolean unification failed.
    */
  private case object BooleanUnificationException extends RuntimeException

  /**
    * Returns `true` if the given boolean formula `f` is satisfiable.
    */
  private def sat(f: Type)(implicit flix: Flix): Boolean = f match {
    case Type.Pure => true
    case Type.Impure => false
    case _ =>
      val q = mkEq(f, Type.Pure)
      try {
        successiveVariableElimination(q, q.typeVars.toList)
        true
      } catch {
        case BooleanUnificationException => false
      }
  }

  /**
    * Returns `true` if `tpe1` is an instance of `tpe2`.
    */
  def isInstance(tpe1: Type, tpe2: Type)(implicit flix: Flix): Boolean =
    Unification.unifyTypes(tpe1, tpe2) match {
      case Ok(_) => true
      case Err(_) => false
    }

  /**
    * Lifts the given value `a` into the type inference monad
    */
  def liftM[A](a: A): InferMonad[A] = InferMonad(s => Ok((s, a)))

  /**
    * Lifts the given value `a` and substitution `s` into the type inference monad..
    */
  def liftM[A](a: A, s: Substitution): InferMonad[A] = InferMonad(_ => Ok(s, a))

  /**
    * Lifts the given error `e` into the type inference monad.
    */
  def failM[A](e: TypeError): InferMonad[A] = InferMonad(_ => Err(e))

  /**
    * Unifies the two given types `tpe1` and `tpe2` lifting their unified types and
    * associated substitution into the type inference monad.
    */
  def unifyTypM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    InferMonad((s: Substitution) => {
      val type1 = s(tpe1)
      val type2 = s(tpe2)
      unifyTypes(type1, type2) match {
        case Result.Ok(s1) =>
          val subst = s1 @@ s
          Ok(subst, subst(tpe1))

        case Result.Err(UnificationError.MismatchedTypes(baseType1, baseType2)) =>
          Err(TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.MismatchedEffects(baseType1, baseType2)) =>
          Err(TypeError.MismatchedEffects(baseType1, baseType2, loc))

        case Result.Err(UnificationError.MismatchedArity(baseType1, baseType2)) =>
          Err(TypeError.MismatchedArity(tpe1, tpe2, loc))

        case Result.Err(UnificationError.RigidVar(baseType1, baseType2)) =>
          Err(TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.OccursCheck(baseType1, baseType2)) =>
          Err(TypeError.OccursCheckError(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.UndefinedLabel(fieldName, fieldType, recordType)) =>
          Err(TypeError.UndefinedField(fieldName, fieldType, recordType, loc))

        case Result.Err(UnificationError.NonRecordType(tpe)) =>
          Err(TypeError.NonRecordType(tpe, loc))

        case Result.Err(UnificationError.UndefinedPredicate(predSym, predType, schemaType)) =>
          Err(TypeError.UndefinedPredicate(predSym, predType, schemaType, loc))

        case Result.Err(UnificationError.NonSchemaType(tpe)) =>
          Err(TypeError.NonSchemaType(tpe, loc))
      }
    }
    )
  }

  /**
    * Unifies the three given types `tpe1`, `tpe2`, and `tpe3`.
    */
  def unifyTypM(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyTypM(List(tpe1, tpe2, tpe3), loc)

  /**
    * Unifies the four given types `tpe1`, `tpe2`, `tpe3` and `tpe4`.
    */
  def unifyTypM(tpe1: Type, tpe2: Type, tpe3: Type, tpe4: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyTypM(List(tpe1, tpe2, tpe3, tpe4), loc)

  /**
    * Unifies all the types in the given non-empty list `ts`.
    */
  def unifyTypM(ts: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    def visit(x0: InferMonad[Type], xs: List[Type]): InferMonad[Type] = xs match {
      case Nil => x0
      case y :: ys => x0 flatMap {
        case tpe => visit(unifyTypM(tpe, y, loc), ys)
      }
    }

    visit(liftM(ts.head), ts.tail)
  }

  /**
    * Unifies all the types in the given (possibly empty) list `ts`.
    */
  def unifyTypAllowEmptyM(ts: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    if (ts.isEmpty)
      liftM(Type.freshTypeVar())
    else
      unifyTypM(ts, loc)
  }

  /**
    * Pairwise unifies the two given lists of types `xs` and `ys`.
    */
  def unifyTypM(xs: List[Type], ys: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[List[Type]] = seqM((xs zip ys).map {
    case (x, y) => unifyTypM(x, y, loc)
  })

  /**
    * Unifies the two given effects `eff1` and `eff2`.
    */
  def unifyEffM(eff1: Type, eff2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    // Determine if effect checking is enabled.
    if (flix.options.xnoeffects)
      return liftM(Type.Pure)

    InferMonad((s: Substitution) => {
      val effect1 = s(eff1)
      val effect2 = s(eff2)
      unifyEffects(effect1, effect2) match {
        case Result.Ok(s1) =>
          val subst = s1 @@ s
          Ok(subst, subst(eff1))

        case Result.Err(e) => e match {
          case UnificationError.MismatchedEffects(baseType1, baseType2) =>
            Err(TypeError.MismatchedEffects(baseType1, baseType2, loc))

          case _ => throw InternalCompilerException(s"Unexpected error: '$e'.")
        }
      }
    }
    )
  }

  /**
    * Unifies the three given effects `eff1`, `eff2`, and `eff3`.
    */
  def unifyEffM(eff1: Type, eff2: Type, eff3: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyEffM(List(eff1, eff2, eff3), loc)

  /**
    * Unifies the four given effects `eff1`, `eff2`, `eff3`, and `eff4`.
    */
  def unifyEffM(eff1: Type, eff2: Type, eff3: Type, eff4: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyEffM(List(eff1, eff2, eff3, eff4), loc)

  /**
    * Unifies all the effects in the given non-empty list `fs`.
    */
  def unifyEffM(fs: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    def visit(x0: InferMonad[Type], xs: List[Type]): InferMonad[Type] = xs match {
      case Nil => x0
      case y :: ys => x0 flatMap {
        case tpe => visit(unifyEffM(tpe, y, loc), ys)
      }
    }

    visit(liftM(fs.head), fs.tail)
  }

  /**
    * Collects the result of each type inference monad in `ts` going left to right.
    */
  def seqM[A](xs: List[InferMonad[A]]): InferMonad[List[A]] = xs match {
    case Nil => liftM(Nil)
    case y :: ys => y flatMap {
      case r => seqM(ys) map {
        case rs => r :: rs
      }
    }
  }

  /**
    * Returns the negation of the effect `eff0`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  private def mkNot(eff0: Type): Type = eff0 match {
    case Type.Pure =>
      Type.Impure

    case Type.Impure =>
      Type.Pure

    case NOT(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case OR(NOT(x), y) =>
      mkAnd(x, mkNot(y))

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case OR(x, NOT(y)) =>
      mkAnd(mkNot(x), y)

    case _ => Type.Apply(Type.Cst(TypeConstructor.Not), eff0)
  }

  /**
    * Returns the conjunction of the two effects `eff1` and `eff2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  private def mkAnd(eff1: Type, eff2: Type): Type = (eff1, eff2) match {
    // T ∧ x => x
    case (Type.Pure, _) =>
      eff2

    // x ∧ T => x
    case (_, Type.Pure) =>
      eff1

    // F ∧ x => F
    case (Type.Impure, _) =>
      Type.Impure

    // x ∧ F => F
    case (_, Type.Impure) =>
      Type.Impure

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (NOT(x1), OR(x2, y)) if x1 == x2 =>
      mkAnd(mkNot(x1), y)

    // x ∧ ¬x => F
    case (x1, NOT(x2)) if x1 == x2 =>
      Type.Impure

    // ¬x ∧ x => F
    case (NOT(x1), x2) if x1 == x2 =>
      Type.Impure

    // x ∧ (x ∧ y) => (x ∧ y)
    case (x1, AND(x2, y)) if x1 == x2 =>
      mkAnd(x1, y)

    // x ∧ (y ∧ x) => (x ∧ y)
    case (x1, AND(y, x2)) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (AND(x1, y), x2) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (AND(x, y1), y2) if y1 == y2 =>
      mkAnd(x, y1)

    // x ∧ (x ∨ y) => x
    case (x1, OR(x2, _)) if x1 == x2 =>
      x1

    // (x ∨ y) ∧ x => x
    case (OR(x1, _), x2) if x1 == x2 =>
      x1

    // x ∧ (y ∧ ¬x) => F
    case (x1, AND(_, NOT(x2))) if x1 == x2 =>
      Type.Impure

    // (¬x ∧ y) ∧ x => F
    case (AND(NOT(x1), _), x2) if x1 == x2 =>
      Type.Impure

    // x ∧ ¬(x ∨ y) => F
    case (x1, NOT(OR(x2, _))) if x1 == x2 =>
      Type.Impure

    // ¬(x ∨ y) ∧ x => F
    case (NOT(OR(x1, _)), x2) if x1 == x2 =>
      Type.Impure

    // x ∧ (¬x ∧ y) => F
    case (x1, AND(NOT(x2), _)) if x1 == x2 =>
      Type.Impure

    // (¬x ∧ y) ∧ x => F
    case (AND(NOT(x1), _), x2) if x1 == x2 =>
      Type.Impure

    // x ∧ x => x
    case _ if eff1 == eff2 => eff1

    case _ =>
      //      val s = s"And($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And), eff1), eff2)
  }

  /**
    * Returns the disjunction of the two effects `eff1` and `eff2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  @tailrec
  private def mkOr(eff1: Type, eff2: Type): Type = (eff1, eff2) match {
    // T ∨ x => T
    case (Type.Pure, _) =>
      Type.Pure

    // F ∨ y => y
    case (Type.Impure, _) =>
      eff2

    // x ∨ T => T
    case (_, Type.Pure) =>
      Type.Pure

    // x ∨ F => x
    case (_, Type.Impure) =>
      eff1

    // x ∨ (y ∨ x) => x ∨ y
    case (x1, OR(y, x2)) if x1 == x2 =>
      mkOr(x1, y)

    // (x ∨ y) ∨ x => x ∨ y
    case (OR(x1, y), x2) if x1 == x2 =>
      mkOr(x1, y)

    // ¬x ∨ x => T
    case (NOT(x), y) if x == y =>
      Type.Pure

    // x ∨ ¬x => T
    case (x, NOT(y)) if x == y =>
      Type.Pure

    // (¬x ∨ y) ∨ x) => T
    case (OR(NOT(x), _), y) if x == y =>
      Type.Pure

    // x ∨ (¬x ∨ y) => T
    case (x, OR(NOT(y), _)) if x == y =>
      Type.Pure

    // x ∨ (y ∧ x) => x
    case (x1, AND(_, x2)) if x1 == x2 => x1

    // (y ∧ x) ∨ x => x
    case (AND(_, x1), x2) if x1 == x2 => x1

    // x ∨ x => x
    case _ if eff1 == eff2 =>
      eff1

    case _ =>

      //              val s = s"Or($eff1, $eff2)"
      //              val len = s.length
      //              if (len > 30) {
      //                println(s.substring(0, Math.min(len, 300)))
      //              }

      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or), eff1), eff2)
  }

  object NOT {
    def unapply(eff: Type): Option[Type] = eff match {
      case Type.Apply(Type.Cst(TypeConstructor.Not), x) => Some(x)
      case _ => None
    }
  }

  object AND {
    def unapply(eff: Type): Option[(Type, Type)] = eff match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And), x), y) => Some((x, y))
      case _ => None
    }
  }

  object OR {
    def unapply(eff: Type): Option[(Type, Type)] = eff match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or), x), y) => Some((x, y))
      case _ => None
    }
  }

}
