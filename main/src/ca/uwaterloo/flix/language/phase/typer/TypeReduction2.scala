/*
 * Copyright 2024 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.util.JvmUtils
import ca.uwaterloo.flix.util.collection.ListMap
import org.apache.commons.lang3.reflect.{ConstructorUtils, MethodUtils}

import java.lang.reflect.{Constructor, Field, Method}

object TypeReduction2 {

  /**
    * Performs various reduction rules on the given type.
    */
  def reduce(tpe0: Type, scope: Scope, renv: RigidityEnv)(implicit progress: Progress, eqenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Type = tpe0 match {
    case t: Type.Var => t

    case t: Type.Cst => t

    case Type.Apply(tpe1, tpe2, loc) =>
      // TODO CONSTR-SOLVER-2 this is recursive. Might be a hot-spot for performance?
      val t1 = reduce(tpe1, scope, renv)
      val t2 = reduce(tpe2, scope, renv)
      Type.Apply(t1, t2, loc)

    case Type.Alias(_, _, tpe, _) => tpe

    case Type.AssocType(Ast.AssocTypeConstructor(sym, _), tpe, _, _) =>

      // Get all the associated types from the context
      val assocs = eqenv(sym)

      // Find the instance that matches
      val matches = assocs.flatMap {
        case Ast.AssocTypeDef(assocTpe, ret) =>
          // We fully rigidify `tpe`, because we need the substitution to go from instance type to constraint type.
          // For example, if our constraint is ToString[Map[Int32, a]] and our instance is ToString[Map[k, v]],
          // then we want the substitution to include "v -> a" but NOT "a -> v".
          val assocRenv = tpe.typeVars.map(_.sym).foldLeft(renv)(_.markRigid(_))

          // Instantiate all the instance constraints according to the substitution.
          ConstraintSolver2.fullyUnify(tpe, assocTpe, scope, assocRenv).map {
            case subst => subst(ret)
          }
      }

      // TODO CONSTR-SOLVER-2 ought to be exactly 0 or 1; should check in Resolver
      matches match {
        // Case 1: No match. Can't reduce the type.
        case Nil => tpe0

        // Case 2: One match. Use it.
        case newTpe :: Nil =>
          progress.markProgress()
          newTpe

        // Case 3: Multiple matches. Give back the original type.
        // TODO CONSTR-SOLVER-2 Right resiliency strategy?
        case _ :: _ :: _ => tpe0
      }

    case Type.JvmToType(tpe, loc) =>
      reduce(tpe, scope, renv) match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          progress.markProgress()
          Type.getFlixType(constructor.getDeclaringClass)

        case Type.Cst(TypeConstructor.JvmMethod(method), _) =>
          progress.markProgress()
          Type.getFlixType(method.getReturnType)

        case Type.Cst(TypeConstructor.JvmField(field), _) =>
          progress.markProgress()
          Type.getFlixType(field.getType)

        case t => Type.JvmToType(t, loc)
      }

    case Type.JvmToEff(tpe, loc) =>
      reduce(tpe, scope, renv) match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          progress.markProgress()
          BaseEffects.getConstructorEffs(constructor, loc)

        case Type.Cst(TypeConstructor.JvmMethod(method), _) =>
          progress.markProgress()
          BaseEffects.getMethodEffs(method, loc)

        case t => Type.JvmToType(t, loc)
      }

    case unresolved@Type.UnresolvedJvmType(member, loc) =>
      member.map(reduce(_, scope, renv)) match {
        case JvmMember.JvmConstructor(clazz, tpes) =>
          lookupConstructor(clazz, tpes) match {
            case JavaConstructorResolution.Resolved(constructor) =>
              progress.markProgress()
              Type.Cst(TypeConstructor.JvmConstructor(constructor), loc)
            case _ => unresolved
          }

        case JvmMember.JvmField(tpe, name) =>
          lookupField(tpe, name.name) match {
            case JavaFieldResolution.Resolved(field) =>
              progress.markProgress()
              Type.Cst(TypeConstructor.JvmField(field), loc)
            case _ => unresolved
          }

        case JvmMember.JvmMethod(tpe, name, tpes) =>
          lookupMethod(tpe, name.name, tpes) match {
            case JavaMethodResolution.Resolved(method) =>
              progress.markProgress()
              Type.Cst(TypeConstructor.JvmMethod(method), loc)
            case _ => unresolved
          }

        case JvmMember.JvmStaticMethod(clazz, name, tpes) =>
          lookupStaticMethod(clazz, name.name, tpes) match {
            case JavaMethodResolution.Resolved(method) =>
              progress.markProgress()
              Type.Cst(TypeConstructor.JvmMethod(method), loc)
            case _ => unresolved
          }
      }
  }

  /** Tries to find a constructor of `clazz` that takes arguments of type `ts`. */
  private def lookupConstructor(clazz: Class[?], ts: List[Type]): JavaConstructorResolution = {
    val typesAreKnown = ts.forall(isKnown)
    if (!typesAreKnown) return JavaConstructorResolution.UnresolvedTypes

    val tparams = ts.map(getJavaType)
    val c = ConstructorUtils.getMatchingAccessibleConstructor(clazz, tparams *)

    // Check if we found a matching constructor.
    if (c != null) {
      JavaConstructorResolution.Resolved(c)
    } else {
      JavaConstructorResolution.NotFound
    }
  }

  /** Tries to find a method of `thisObj` that takes arguments of type `ts`. */
  private def lookupMethod(thisObj: Type, methodName: String, ts: List[Type]): JavaMethodResolution = {
    val typesAreKnown = isKnown(thisObj) && ts.forall(isKnown)
    if (!typesAreKnown) return JavaMethodResolution.UnresolvedTypes

    Type.classFromFlixType(thisObj) match {
      case Some(clazz) => retrieveMethod(clazz, methodName, ts, static = false)
      case None => JavaMethodResolution.NotFound
    }
  }

  /** Tries to find a static method of `clazz` that takes arguments of type `ts`. */
  private def lookupStaticMethod(clazz: Class[?], methodName: String, ts: List[Type]): JavaMethodResolution = {
    val typesAreKnown = ts.forall(isKnown)
    if (!typesAreKnown) return JavaMethodResolution.UnresolvedTypes

    retrieveMethod(clazz, methodName, ts, static = true)
  }

  /** Tries to find a static/dynamic method of `clazz` that takes arguments of type `ts`. */
  private def retrieveMethod(clazz: Class[?], methodName: String, ts: List[Type], static: Boolean): JavaMethodResolution = {
    val tparams = ts.map(getJavaType)
    val m = MethodUtils.getMatchingAccessibleMethod(clazz, methodName, tparams *)
    // We check if we found a method and if its static flag matches.
    if (m != null && JvmUtils.isStatic(m) == static) {
      // Case 1: We found the method on the clazz.
      JavaMethodResolution.Resolved(m)
    } else {
      // Case 2: We failed to find the method on the clazz.
      // We make one attempt on java.lang.Object.
      val classObj = classOf[java.lang.Object]
      val m = MethodUtils.getMatchingAccessibleMethod(classObj, methodName, tparams *)
      if (m != null && JvmUtils.isStatic(m) == static) {
        // Case 2.1: We found the method on java.lang.Object.
        JavaMethodResolution.Resolved(m)
      } else {
        // Case 2.2: We failed to find the method, so we report an error on the original clazz.
        JavaMethodResolution.NotFound
      }
    }
  }

  /**
    * Returns the Java reflective class object corresponding to the given Flix `tpe`.
    */
  private def getJavaType(tpe: Type): Class[?] = tpe match {
    case Type.Bool => java.lang.Boolean.TYPE
    case Type.Int8 => java.lang.Byte.TYPE
    case Type.Int16 => java.lang.Short.TYPE
    case Type.Int32 => java.lang.Integer.TYPE
    case Type.Int64 => java.lang.Long.TYPE
    case Type.Char => java.lang.Character.TYPE
    case Type.Float32 => java.lang.Float.TYPE
    case Type.Float64 => java.lang.Double.TYPE
    case Type.Cst(TypeConstructor.BigDecimal, _) => classOf[java.math.BigDecimal]
    case Type.Cst(TypeConstructor.BigInt, _) => classOf[java.math.BigInteger]
    case Type.Cst(TypeConstructor.Str, _) => classOf[String]
    case Type.Cst(TypeConstructor.Regex, _) => classOf[java.util.regex.Pattern]
    case Type.Cst(TypeConstructor.Native(clazz), _) => clazz

    // Arrays
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Array, _), elmType, _), _, _) =>
      // Find the Java class of the element type.
      val t = getJavaType(elmType)

      // Find the Java class of the array type.
      // See: https://stackoverflow.com/questions/1679421/how-to-get-the-array-class-for-a-given-class-in-java
      val phantomArr = java.lang.reflect.Array.newInstance(t, 0)
      val arrType = phantomArr.getClass
      arrType

    // Vectors
    case Type.Apply(Type.Cst(TypeConstructor.Vector, _), elmType, _) =>
      // Find the Java class of the element type.
      val t = getJavaType(elmType)

      // Find the Java class of the array type.
      // See: https://stackoverflow.com/questions/1679421/how-to-get-the-array-class-for-a-given-class-in-java
      val phantomArr = java.lang.reflect.Array.newInstance(t, 0)
      val arrType = phantomArr.getClass
      arrType

    // Functions
    case Type.Apply(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Arrow(2), _), _, _), varArg, _), varRet, _) =>
      (varArg, varRet) match {
        case (Type.Cst(tc1, _), Type.Cst(tc2, _)) =>
          (tc1, tc2) match {
            case (TypeConstructor.Int32, TypeConstructor.Unit) => classOf[java.util.function.IntConsumer]
            case (TypeConstructor.Int32, TypeConstructor.Bool) => classOf[java.util.function.IntPredicate]
            case (TypeConstructor.Int32, TypeConstructor.Int32) => classOf[java.util.function.IntUnaryOperator]
            case (TypeConstructor.Int32, TypeConstructor.Native(obj)) if obj == classOf[Object] => classOf[java.util.function.IntFunction[Object]]
            case (TypeConstructor.Float64, TypeConstructor.Unit) => classOf[java.util.function.DoubleConsumer]
            case (TypeConstructor.Float64, TypeConstructor.Bool) => classOf[java.util.function.DoublePredicate]
            case (TypeConstructor.Float64, TypeConstructor.Float64) => classOf[java.util.function.DoubleUnaryOperator]
            case (TypeConstructor.Float64, TypeConstructor.Native(obj)) if obj == classOf[Object] => classOf[java.util.function.DoubleFunction[Object]]
            case (TypeConstructor.Int64, TypeConstructor.Unit) => classOf[java.util.function.LongConsumer]
            case (TypeConstructor.Int64, TypeConstructor.Bool) => classOf[java.util.function.LongPredicate]
            case (TypeConstructor.Int64, TypeConstructor.Int64) => classOf[java.util.function.LongUnaryOperator]
            case (TypeConstructor.Int64, TypeConstructor.Native(obj)) if obj == classOf[Object] => classOf[java.util.function.LongFunction[Object]]
            case (TypeConstructor.Native(obj), TypeConstructor.Unit) if obj == classOf[Object] => classOf[java.util.function.Consumer[Object]]
            case (TypeConstructor.Native(obj), TypeConstructor.Bool) if obj == classOf[Object] => classOf[java.util.function.Predicate[Object]]
            case _ => classOf[Object] // default
          }
        case _ => classOf[Object] // default
      }
    case _ => classOf[Object] // default
  }

  /** Tries to find a field of `thisObj` with the name `fieldName`. */
  private def lookupField(thisObj: Type, fieldName: String): JavaFieldResolution = {
    val typeIsKnown = isKnown(thisObj)
    if (!typeIsKnown) return JavaFieldResolution.UnresolvedTypes
    val opt = for {
      clazz <- Type.classFromFlixType(thisObj)
      field <- JvmUtils.getField(clazz, fieldName, static = false)
    } yield JavaFieldResolution.Resolved(field)
    opt.getOrElse(JavaFieldResolution.NotFound)
  }

  /** Returns `true` if type is resolved enough for Java resolution. */
  private def isKnown(tpe: Type): Boolean = tpe match {
    case Type.Var(_, _) if tpe.kind == Kind.Eff => true
    case Type.Var(_, _) => false
    case Type.Cst(_, _) => true
    case Type.JvmToType(_, _) => false
    case Type.JvmToEff(_, _) => false
    case Type.UnresolvedJvmType(_, _) => false
    case Type.Apply(t1, t2, _) => isKnown(t1) && isKnown(t2)
    case Type.Alias(_, _, t, _) => isKnown(t)
    case Type.AssocType(_, _, _, _) => false
  }

  /** A lookup result of a Java field. */
  private sealed trait JavaFieldResolution

  private object JavaFieldResolution {

    /** One matching field. */
    case class Resolved(field: Field) extends JavaFieldResolution

    /** No matching field. */
    case object NotFound extends JavaFieldResolution

    /**
      * The types used for the lookup are not resolved enough to decide on a field.
      *
      * This happens if they contain e.g., type variables or associated types.
      */
    case object UnresolvedTypes extends JavaFieldResolution

  }

  /** A lookup result of a Java constructor. */
  private sealed trait JavaConstructorResolution

  private object JavaConstructorResolution {

    /** One matching constructor. */
    case class Resolved(constructor: Constructor[?]) extends JavaConstructorResolution

    /** No matching constructor. */
    case object NotFound extends JavaConstructorResolution

    /**
      * The types used for the lookup are not resolved enough to decide on a constructor.
      *
      * This happens if they contain e.g., type variables or associated types.
      */
    case object UnresolvedTypes extends JavaConstructorResolution

  }

  /** A lookup result of a Java method. */
  private sealed trait JavaMethodResolution

  private object JavaMethodResolution {

    /** One matching method. */
    case class Resolved(method: Method) extends JavaMethodResolution

    /** No matching method. */
    case object NotFound extends JavaMethodResolution

    /**
      * The types used for the lookup are not resolved enough to decide on a method.
      *
      * This happens if they contain e.g., type variables or associated types.
      */
    case object UnresolvedTypes extends JavaMethodResolution

  }
}
