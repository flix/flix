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
import ca.uwaterloo.flix.language.ast.{Eff, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Result._
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

object Unification {

  /**
    * Companion object for the [[Substitution]] class.
    */
  object Substitution {
    /**
      * Returns the empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty, Map.empty)

    /**
      * Returns the singleton substitution mapping the type variable `x` to `tpe`.
      */
    def singleton(x: Type.Var, tpe: Type): Substitution = Substitution(Map(x -> tpe), Map.empty)

    /**
      * Returns the singleton substitution mapping the effect variable `x` to `eff`.
      */
    def singleton(x: Eff.Var, eff: Eff): Substitution = Substitution(Map.empty, Map(x -> eff))
  }

  /**
    * A substitution is a map from type variables to types.
    */
  case class Substitution(typeMap: Map[Type.Var, Type], effectMap: Map[Eff.Var, Eff]) {

    /**
      * Returns `true` if `this` is the empty substitution.
      */
    def isEmpty: Boolean = typeMap.isEmpty && effectMap.isEmpty

    /**
      * Applies `this` substitution to the given type `tpe0`.
      */
    def apply(tpe0: Type): Type = tpe0 match {
      case x: Type.Var =>
        typeMap.get(x) match {
          case None => x
          case Some(y) if x.kind == tpe0.kind => y
          case Some(y) if x.kind != tpe0.kind => throw InternalCompilerException(s"Expected kind `${x.kind}' but got `${tpe0.kind}'.")
        }
      case Type.Cst(tc) => Type.Cst(tc)
      case Type.Arrow(f, l) => Type.Arrow(f, l)
      case Type.RecordEmpty => Type.RecordEmpty
      case Type.RecordExtend(label, field, rest) => Type.RecordExtend(label, apply(field), apply(rest))
      case Type.SchemaEmpty => Type.SchemaEmpty
      case Type.SchemaExtend(sym, tpe, rest) => Type.SchemaExtend(sym, apply(tpe), apply(rest))
      case Type.Zero => Type.Zero
      case Type.Succ(n, t) => Type.Succ(n, apply(t))
      case Type.Relation(sym, attr, kind) => Type.Relation(sym, attr map apply, kind)
      case Type.Lattice(sym, attr, kind) => Type.Lattice(sym, attr map apply, kind)
      case Type.Apply(t1, t2) => Type.Apply(apply(t1), apply(t2))
    }

    /**
      * Applies `this` substitution to the given types `ts`.
      */
    def apply(ts: List[Type]): List[Type] = ts map apply

    /**
      * Applies `this` substitution to the given effect `eff`.
      */
    def apply(eff: Eff): Eff = eff match {
      case x: Eff.Var => effectMap.get(x) match {
        case None => x
        case Some(y) => y
      }
      case Eff.Pure => Eff.Pure
      case Eff.Impure => Eff.Impure
    }

    /**
      * Returns the left-biased composition of `this` substitution with `that` substitution.
      */
    def ++(that: Substitution): Substitution = {
      Substitution(
        this.typeMap ++ that.typeMap.filter(kv => !this.typeMap.contains(kv._1)),
        this.effectMap ++ that.effectMap.filter(kv => !this.effectMap.contains(kv._1))
      )
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution.
      */
    def @@(that: Substitution): Substitution = {
      val newTypeMap = that.typeMap.foldLeft(Map.empty[Type.Var, Type]) {
        case (macc, (x, t)) => macc.updated(x, this.apply(t))
      }
      val newEffectMap = that.effectMap.foldLeft(Map.empty[Eff.Var, Eff]) {
        case (macc, (x, t)) => macc.updated(x, this.apply(t))
      }
      Substitution(newTypeMap, newEffectMap) ++ this
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
    * A common super-type for unification errors.
    */
  sealed trait UnificationError

  object UnificationError {

    /**
      * An unification error due to a mismatch between `tpe1` and `tpe2`.
      *
      * @param tpe1 the first type.
      * @param tpe2 the second type.
      */
    case class Mismatch(tpe1: Type, tpe2: Type) extends UnificationError

    /**
      * An unification error due to an occurrence of `tvar` in `tpe`.
      *
      * @param tvar the type variable.
      * @param tpe  the type.
      */
    case class OccursCheck(tvar: Type.Var, tpe: Type) extends UnificationError

    /**
      * An unification error due the field `fieldName` of type `fieldType` missing from the type `recordType`.
      *
      * @param fieldName  the name of the missing field.
      * @param fieldType  the type of the missing field.
      * @param recordType the record type where the field is missing.
      */
    case class UndefinedLabel(fieldName: String, fieldType: Type, recordType: Type) extends UnificationError

    /**
      * An unification error due the predicate `sym` of type `predType` missing from the type `schemaType`.
      *
      * @param predSym    the symbol of the missing predicate.
      * @param predType   the type of the missing predicate.
      * @param schemaType the schema type where the predicate is missing.
      */
    case class UndefinedPredicate(predSym: Symbol.PredSym, predType: Type, schemaType: Type) extends UnificationError

    /**
      * An unification error due to an unexpected non-record type.
      *
      * @param nonRecordType the unexpected non-record type.
      */
    case class NonRecordType(nonRecordType: Type) extends UnificationError

    /**
      * An unification error due to an unexpected non-schema type.
      *
      * @param nonSchemaType the unexpected non-schema type.
      */
    case class NonSchemaType(nonSchemaType: Type) extends UnificationError

    // TODO: DOC
    case class MismatchedEffects(eff1: Eff, eff2: Eff) extends UnificationError

  }

  /**
    * Returns the most general unifier of the two given types `tpe1` and `tpe2`.
    */
  def unifyTypes(tpe1: Type, tpe2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {

    // NB: Uses a closure to capture the source location `loc`.

    /**
      * Unifies the given variable `x` with the given type `tpe`.
      *
      * Performs the so-called occurs-check to ensure that the substitution is kind-preserving.
      */
    def unifyVar(x: Type.Var, tpe: Type): Result[Substitution, UnificationError] = {
      // The type variable and type are in fact the same.
      if (x == tpe) {
        return Result.Ok(Substitution.empty)
      }

      // The type variable occurs inside the type.
      if (tpe.typeVars contains x) {
        return Result.Err(UnificationError.OccursCheck(x, tpe))
      }

      // TODO: Kinds disabled for now. Requires changed to the previous phase to associated type variables with their kinds.
      //if (x.kind != tpe.kind) {
      //  return Result.Err(TypeError.KindError())
      //}

      // We can substitute `x` for `tpe`. Update the textual name of `tpe`.
      if (x.getText.nonEmpty && tpe.isInstanceOf[Type.Var]) {
        tpe.asInstanceOf[Type.Var].setText(x.getText.get)
      }
      Result.Ok(Substitution.singleton(x, tpe))
    }

    /**
      * Unifies the two given types `tpe1` and `tpe2`.
      */
    def unifyTypes(tpe1: Type, tpe2: Type): Result[Substitution, UnificationError] = (tpe1, tpe2) match {
      case (x: Type.Var, _) => unifyVar(x, tpe2)

      case (_, x: Type.Var) => unifyVar(x, tpe1)

      case (Type.Cst(TypeConstructor.Native(clazz1)), Type.Cst(TypeConstructor.Native(clazz2))) =>
        if (clazz1 == clazz2)
          Result.Ok(Substitution.empty)
        else
          Result.Err(UnificationError.Mismatch(tpe1, tpe2))

      case (Type.Cst(c1), Type.Cst(c2)) =>
        if (c1 == c2)
          Result.Ok(Substitution.empty)
        else
          Result.Err(UnificationError.Mismatch(tpe1, tpe2))

      case (Type.Arrow(f1, l1), Type.Arrow(f2, l2)) if l1 == l2 =>
        unifyEffects(f1, f2)

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

      case (Type.Relation(sym1, attr1, kind1), Type.Relation(sym2, attr2, kind2)) if sym1 == sym2 => unifyAll(attr1, attr2)

      case (Type.Lattice(sym1, attr1, kind1), Type.Lattice(sym2, attr2, kind2)) if sym1 == sym2 => unifyAll(attr1, attr2)

      case (Type.Apply(t11, t12), Type.Apply(t21, t22)) =>
        unifyTypes(t11, t21) match {
          case Result.Ok(subst1) => unifyTypes(subst1(t12), subst1(t22)) match {
            case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
            case Result.Err(e) => Result.Err(e)
          }
          case Result.Err(e) => Result.Err(e)
        }
      case _ => Result.Err(UnificationError.Mismatch(tpe1, tpe2))
    }

    /**
      * Unifies the two given lists of types `ts1` and `ts2`.
      */
    def unifyAll(ts1: List[Type], ts2: List[Type]): Result[Substitution, UnificationError] = (ts1, ts2) match {
      case (Nil, Nil) => Result.Ok(Substitution.empty)
      case (t1 :: rs1, t2 :: rs2) => unifyTypes(t1, t2) match {
        case Result.Ok(subst1) => unifyAll(subst1(rs1), subst1(rs2)) match {
          case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
          case Result.Err(e) => Result.Err(e)
        }
        case Result.Err(e) => Result.Err(e)
      }
      case _ => throw InternalCompilerException(s"Mismatched type lists: `$ts1' and `$ts2'.")
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
    def rewriteSchemaRow(row2: Type, label1: Symbol.PredSym, fieldType1: Type, originalType: Type): Result[(Substitution, Type), UnificationError] = row2 match {
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
  def unifyEffects(eff1: Eff, eff2: Eff): Result[Substitution, UnificationError] = (eff1, eff2) match {
    case (x: Eff.Var, _) => Ok(Substitution.singleton(x, eff2))
    case (_, y: Eff.Var) => Ok(Substitution.singleton(y, eff1))
    case (Eff.Pure, Eff.Pure) => Ok(Substitution.empty)
    case (Eff.Impure, Eff.Impure) => Ok(Substitution.empty)
    case _ => Err(UnificationError.MismatchedEffects(eff1, eff2)) // TODO
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

        case Result.Err(UnificationError.Mismatch(baseType1, baseType2)) =>
          Err(TypeError.UnificationError(baseType1, baseType2, type1, type2, loc))

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

          // TODO: Where in the order
        case Result.Err(UnificationError.MismatchedEffects(eff1, eff2)) =>
          Err(TypeError.MismatchedEffects(eff1, eff2, loc))

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
  def unifyEffM(eff1: Eff, eff2: Eff, loc: SourceLocation)(implicit flix: Flix): InferMonad[Eff] = {
    InferMonad((s: Substitution) => {
      val effect1 = s(eff1)
      val effect2 = s(eff2)
      unifyEffects(effect1, effect2) match {
        case Result.Ok(s1) =>
          val subst = s1 @@ s
          Ok(subst, subst(eff1))

        case Result.Err(UnificationError.MismatchedEffects(e1, e2)) =>
          Err(TypeError.MismatchedEffects(e1, e2, loc)) // TODO
      }
    }
    )
  }

  /**
    * Unifies the three given effects `eff1`, `eff2`, and `eff3`.
    */
  def unifyEffM(eff1: Eff, eff2: Eff, eff3: Eff, loc: SourceLocation)(implicit flix: Flix): InferMonad[Eff] = unifyEffM(List(eff1, eff2, eff3), loc)

  /**
    * Unifies the four given effects `eff1`, `eff2`, `eff3`, and `eff4`.
    */
  def unifyEffM(eff1: Eff, eff2: Eff, eff3: Eff, eff4: Eff, loc: SourceLocation)(implicit flix: Flix): InferMonad[Eff] = unifyEffM(List(eff1, eff2, eff3, eff4), loc)

  /**
    * Unifies all the effects in the given non-empty list `fs`.
    */
  def unifyEffM(fs: List[Eff], loc: SourceLocation)(implicit flix: Flix): InferMonad[Eff] = {
    def visit(x0: InferMonad[Eff], xs: List[Eff]): InferMonad[Eff] = xs match {
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
}
