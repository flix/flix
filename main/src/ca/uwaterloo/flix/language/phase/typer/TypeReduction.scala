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
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Ast, Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.jvm.JvmOps
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.collection.{ListMap, ListOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import org.apache.commons.lang3.reflect.MethodUtils

import java.lang.reflect.{Constructor, Field, Method}
import scala.annotation.tailrec

object TypeReduction {

  /**
    * Simplifies the given type by reducing associated type applications.
    *
    * Θ ⊩ τ ⤳ τ'
    *
    * Returns the simplified type and a Boolean flag to indicate whether progress was made.
    *
    * Applications that cannot be resolved are left as they are.
    * These are applications to variables and applications to other unresolvable types.
    *
    * Applications that are illegal result in an Err.
    * These are applications to types for which the eqEnv has no corresponding instance.
    *
    * For example:
    * {{{
    *   Int           ~> Int
    *   Elm[List[a]]  ~> a
    *   Elm[Int]      ~> <ERROR>
    *   Elm[Elm[a]]   ~> Elm[Elm[a]]
    * }}}
    */
  def simplify(tpe: Type, renv0: RigidityEnv, loc: SourceLocation)(implicit scope: Scope, eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[(Type, Boolean), TypeError] = tpe match {
    // A var is already simple.
    case t: Type.Var => Result.Ok((t, false))

    // A constant is already simple
    case t: Type.Cst => Result.Ok((t, false))

    // lapp_L and lapp_R
    case Type.Apply(tpe1, tpe2, loc) =>
      for {
        (t1, p1) <- simplify(tpe1, renv0, loc)
        (t2, p2) <- simplify(tpe2, renv0, loc)
      } yield {
        (Type.Apply(t1, t2, loc), p1 || p2)
      }

    // arg_R and syn_R
    case Type.AssocType(cst, arg, kind, _) =>
      simplify(arg, renv0, loc).flatMap {
        case (t, p) =>
          // we mark t's tvars as rigid so we get the substitution in the right direction
          val renv = t.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
          val insts = eenv(cst.sym)

          // find the first (and only) instance that matches
          val simplifiedOpt = ListOps.findMap(insts) {
            inst =>
              Unification.unifyTypes(t, inst.arg, renv).toOption.flatMap {
                case (subst, Nil) => Some(subst(inst.ret))
                case (_, _ :: _) => None // if we have leftover constraints then it didn't actually unify
              }
          }
          simplifiedOpt match {
            // Can't reduce. Check what the original type was.
            case None =>
              t.baseType match {
                // If it's a var, it's ok. It may be substituted later to a type we can reduce.
                // Or it might be part of the signature as an associated type.
                case Type.Var(_, loc) => Result.Ok((Type.AssocType(cst, t, kind, loc), p))
                // If it's an associated type, it's ok. It may be reduced later to a concrete type.
                case _: Type.AssocType => Result.Ok((Type.AssocType(cst, t, kind, loc), p))
                // Otherwise it's a problem.
                case baseTpe => Result.Err(ConstraintSolver.mkMissingInstance(cst.sym.trt, baseTpe, renv, loc))
              }
            // We could reduce! Simplify further if possible.
            case Some(t) => simplify(t, renv0, loc).map { case (res, _) => (res, true) }
          }
      }

    case Type.Alias(_, _, t, _) => simplify(t, renv0, loc)

    case Type.JvmToType(j0, _) =>
      simplify(j0, renv0, loc).map {
        case (Type.Cst(TypeConstructor.JvmConstructor(constructor), _), _) => (Type.getFlixType(constructor.getDeclaringClass), true)
        case (Type.Cst(TypeConstructor.JvmField(field), _), _) => (Type.getFlixType(field.getType), true)
        case (Type.Cst(TypeConstructor.JvmMethod(method), _), _) => (Type.getFlixType(method.getReturnType), true)
        case (j, p) => (Type.JvmToType(j, loc), p)
      }

    case Type.JvmToEff(j0, _) =>
      simplify(j0, renv0, loc).map {
        case (Type.Cst(TypeConstructor.JvmConstructor(constructor), loc), _) =>
          (BaseEffects.getConstructorEffs(constructor, loc), true)

        case (Type.Cst(TypeConstructor.JvmMethod(method), _), _) =>
          (BaseEffects.getMethodEffs(method, loc), true)

        case (Type.Cst(TypeConstructor.JvmField(_), _), _) =>
          // Fields should never have any effect other than IO.
          throw InternalCompilerException("Unexpected field effect", loc)

        case (j, p) => (Type.JvmToEff(j, loc), p)
      }

    case cons@Type.UnresolvedJvmType(Type.JvmMember.JvmConstructor(clazz, tpes), _) =>
      lookupConstructor(clazz, tpes, loc) match {
        case JavaConstructorResolution.Resolved(constructor) =>
          val tpe = Type.Cst(TypeConstructor.JvmConstructor(constructor), loc)
          Result.Ok((tpe, true))
        case JavaConstructorResolution.AmbiguousConstructor(constructors) =>
          Result.Err(TypeError.AmbiguousConstructor(clazz, tpes, constructors, renv0, loc))
        case JavaConstructorResolution.NotFound =>
          // TODO INTEROP: fill in candidate methods
          Result.Err(TypeError.ConstructorNotFound(clazz, tpes, List(), renv0, loc))
        case JavaConstructorResolution.UnresolvedTypes =>
          Result.Ok(cons, false)
      }

    case meth@Type.UnresolvedJvmType(Type.JvmMember.JvmMethod(tpe, name, tpes), _) =>
      lookupMethod(tpe, name.name, tpes, loc) match {
        case JavaMethodResolution.Resolved(method) =>
          val tpe = Type.Cst(TypeConstructor.JvmMethod(method), loc)
          Result.Ok((tpe, true))
        case JavaMethodResolution.AmbiguousMethod(methods) =>
          Result.Err(TypeError.AmbiguousMethod(name, tpe, tpes, methods, renv0, loc))
        case JavaMethodResolution.NotFound =>
          Result.Err(TypeError.MethodNotFound(name, tpe, tpes, loc))
        case JavaMethodResolution.UnresolvedTypes =>
          Result.Ok((meth, false))
      }

    case meth@Type.UnresolvedJvmType(Type.JvmMember.JvmStaticMethod(clazz, name, tpes), _) =>
      lookupStaticMethod(clazz, name.name, tpes, loc) match {
        case JavaMethodResolution.Resolved(method) =>
          val tpe = Type.Cst(TypeConstructor.JvmMethod(method), loc)
          Result.Ok((tpe, true))
        case JavaMethodResolution.AmbiguousMethod(methods) =>
          Result.Err(TypeError.AmbiguousStaticMethod(clazz, name, tpes, methods, renv0, loc))
        case JavaMethodResolution.NotFound =>
          // TODO INTEROP: fill in candidate methods
          Result.Err(TypeError.StaticMethodNotFound(clazz, name, tpes, List(), renv0, loc))
        case JavaMethodResolution.UnresolvedTypes =>
          Result.Ok((meth, false))
      }

    case field@Type.UnresolvedJvmType(Type.JvmMember.JvmField(tpe, name), _) =>
      lookupField(tpe, name.name) match {
        case JavaFieldResolution.Resolved(field) =>
          val tpe = Type.Cst(TypeConstructor.JvmField(field), loc)
          Result.Ok((tpe, true))
        case JavaFieldResolution.NotFound =>
          Result.Err(TypeError.FieldNotFound(name, tpe, loc))
        case JavaFieldResolution.UnresolvedTypes =>
          Result.Ok((field, false))
      }
  }

  /** A lookup result of a Java constructor. */
  private sealed trait JavaConstructorResolution

  private object JavaConstructorResolution {

    /** One matching constructor. */
    case class Resolved(constructor: Constructor[?]) extends JavaConstructorResolution

    /** Many matching constructors. */
    case class AmbiguousConstructor(constructors: List[Constructor[?]]) extends JavaConstructorResolution

    /** No matching constructor. */
    case object NotFound extends JavaConstructorResolution

    /**
      * The types of the lookup are not resolved enough to decide
      * (they contain type variables, associated types, etc.).
      */
    case object UnresolvedTypes extends JavaConstructorResolution

  }

  /** Tries to find a constructor of `clazz` that takes arguments of type `ts`. */
  private def lookupConstructor(clazz: Class[?], ts: List[Type], loc: SourceLocation): JavaConstructorResolution = {
    val typesAreKnown = ts.forall(isKnown)
    if (!typesAreKnown) return JavaConstructorResolution.UnresolvedTypes

    val candidates = clazz.getConstructors.toList.filter(isCandidateConstructor(_, ts))

    candidates match {
      case Nil => JavaConstructorResolution.NotFound
      case constructor :: Nil => JavaConstructorResolution.Resolved(constructor)
      case _ :: _ :: _ =>
        // Multiple candidate constructors exist according to subtyping, so we search for an exact
        // match. Candidates could contain `append(String)` and `append(Object)` for the call
        // `append("a")`.
        val exactMatches = candidates.filter(c => exactArguments(c.getParameterTypes, ts))

        exactMatches match {
          // No exact matches - we have ambiguity among the candidates.
          case Nil => JavaConstructorResolution.AmbiguousConstructor(candidates)

          case constructor :: Nil => JavaConstructorResolution.Resolved(constructor)

          // Multiple exact matches are impossible in Java.
          case _ :: _ :: _ =>
            throw InternalCompilerException("Unexpected multiple exact matches for Java constructor", loc)
        }
    }
  }

  /** Returns `true` if `constructor` can be called with arguments types in `ts` according to subtyping. */
  private def isCandidateConstructor(constructor: Constructor[?], ts: List[Type]): Boolean =
    subtypeArguments(constructor.getParameterTypes, ts)

  /** A lookup result of a Java method. */
  private sealed trait JavaMethodResolution

  private object JavaMethodResolution {

    /** One matching method. */
    case class Resolved(method: Method) extends JavaMethodResolution

    /** Many matching methods */
    case class AmbiguousMethod(methods: List[Method]) extends JavaMethodResolution

    /** No matching method. */
    case object NotFound extends JavaMethodResolution

    /**
      * The types of the lookup are not resolved enough to decide
      * (they contain type variables, associated types, etc.).
      */
    case object UnresolvedTypes extends JavaMethodResolution

  }

  /** Tries to find a method of `thisObj` that takes arguments of type `ts`. */
  private def lookupMethod(thisObj: Type, methodName: String, ts: List[Type], loc: SourceLocation): JavaMethodResolution = {
    val typesAreKnown = isKnown(thisObj) && ts.forall(isKnown)
    if (!typesAreKnown) return JavaMethodResolution.UnresolvedTypes

    Type.classFromFlixType(thisObj) match {
      case Some(clazz) => retrieveMethod(clazz, methodName, ts, static = false, loc)
      case None => JavaMethodResolution.NotFound
    }
  }

  /** Tries to find a static method of `clazz` that takes arguments of type `ts`. */
  private def lookupStaticMethod(clazz: Class[?], methodName: String, ts: List[Type], loc: SourceLocation): JavaMethodResolution = {
    val typesAreKnown = ts.forall(isKnown)
    if (!typesAreKnown) return JavaMethodResolution.UnresolvedTypes
    retrieveMethod(clazz, methodName, ts, static = true, loc)
  }

  /** Tries to find a static/dynamic method of `clazz` that takes arguments of type `ts`. */
  private def retrieveMethod(clazz: Class[?], methodName: String, ts: List[Type], static: Boolean, loc: SourceLocation): JavaMethodResolution = {
    val tparams = ts.map(getJavaType)
    val m = MethodUtils.getMatchingAccessibleMethod(clazz, methodName, tparams *)
    if (m != null) {
      // Case 1: We found the method on the clazz.
      JavaMethodResolution.Resolved(m)
    } else {
      // Case 2: We failed to find the method on the clazz.
      // We make one attempt on java.lang.Object.
      val classObj = classOf[java.lang.Object]
      val m = MethodUtils.getMatchingAccessibleMethod(classObj, methodName, tparams *)
      if (m != null) {
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

  /** Returns `true` if the `arguments` types exactly match the `params` types. */
  private def exactArguments(params: Iterable[Class[?]], arguments: Iterable[Type]): Boolean = {
    params.corresponds(arguments) { case (p, a) => Type.getFlixType(p) == a }
  }

  /** Returns `true` if the `arguments` types are subtypes of the `params` types. */
  private def subtypeArguments(params: Iterable[Class[?]], arguments: Iterable[Type]): Boolean = {
    params.corresponds(arguments) { case (p, a) => isSubtype(a, Type.getFlixType(p)) }
  }

  /** A lookup result of a Java field. */
  private sealed trait JavaFieldResolution

  private object JavaFieldResolution {

    /** One matching field. */
    case class Resolved(field: Field) extends JavaFieldResolution

    /** No matching field. */
    case object NotFound extends JavaFieldResolution

    /**
      * The types of the lookup are not resolved enough to decide
      * (it contains type variables, associated types, etc.).
      */
    case object UnresolvedTypes extends JavaFieldResolution

  }

  /** Tries to find a field of `thisObj` with the name `fieldName`. */
  private def lookupField(thisObj: Type, fieldName: String): JavaFieldResolution = {
    val typeIsKnown = isKnown(thisObj)
    if (!typeIsKnown) return JavaFieldResolution.UnresolvedTypes
    Type.classFromFlixType(thisObj) match {
      case Some(clazz) => retrieveField(clazz, fieldName)
      case None => JavaFieldResolution.NotFound
    }
  }

  /** Tries to find a field of `clazz` with the name `fieldName`. */
  private def retrieveField(clazz: Class[?], fieldName: String): JavaFieldResolution = {
    JvmOps.getField(clazz, fieldName) match {
      case Some(field) => JavaFieldResolution.Resolved(field)
      case None => JavaFieldResolution.NotFound
    }
  }

  /**
    * Returns `true` if `tpe1` is a Java subtype of `tpe2`.
    *
    * OBS: the given types must be known according to [[isKnown]].
    */
  @tailrec
  private def isSubtype(tpe1: Type, tpe2: Type): Boolean = {
    (tpe1, tpe2) match {
      case (t1, t2) if t1 == t2 => true
      // Base types
      case (Type.Cst(TypeConstructor.Native(clazz1), _), Type.Cst(TypeConstructor.Native(clazz2), _)) =>
        clazz2.isAssignableFrom(clazz1)
      case (Type.Cst(TypeConstructor.Unit, _), Type.Cst(TypeConstructor.Native(clazz), _)) =>
        clazz == classOf[java.lang.Object]
      case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(clazz), _)) =>
        clazz.isAssignableFrom(classOf[java.lang.String])
      case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(clazz), _)) =>
        clazz.isAssignableFrom(classOf[java.math.BigInteger])
      case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(clazz), _)) =>
        clazz.isAssignableFrom(classOf[java.math.BigDecimal])
      case (Type.Cst(TypeConstructor.Regex, _), Type.Cst(TypeConstructor.Native(clazz), _)) =>
        clazz.isAssignableFrom(classOf[java.util.regex.Pattern])
      // Arrays
      case (Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Array, _), elmType1, _), _, _),
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Array, _), elmType2, _), _, _)) =>
        isSubtype(elmType1, elmType2)
      // Arrow to Java function interface
      case (Type.Apply(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Arrow(2), _), _, _), varArg, _), varRet, _), Type.Cst(TypeConstructor.Native(clazz), _)) =>
        (varArg, varRet) match {
          case (Type.Cst(tc1, _), Type.Cst(tc2, _)) =>
            (tc1, tc2) match {
              case (TypeConstructor.Int32, TypeConstructor.Unit) =>
                clazz == classOf[java.util.function.IntConsumer]
              case (TypeConstructor.Int32, TypeConstructor.Bool) =>
                clazz == classOf[java.util.function.IntPredicate]
              case (TypeConstructor.Int32, TypeConstructor.Int32) =>
                clazz == classOf[java.util.function.IntUnaryOperator]
              case (TypeConstructor.Int32, TypeConstructor.Native(obj)) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.IntFunction[Object]]
              case (TypeConstructor.Float64, TypeConstructor.Unit) =>
                clazz == classOf[java.util.function.DoubleConsumer]
              case (TypeConstructor.Float64, TypeConstructor.Bool) =>
                clazz == classOf[java.util.function.DoublePredicate]
              case (TypeConstructor.Float64, TypeConstructor.Float64) =>
                clazz == classOf[java.util.function.DoubleUnaryOperator]
              case (TypeConstructor.Float64, TypeConstructor.Native(obj)) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.DoubleFunction[Object]]
              case (TypeConstructor.Int64, TypeConstructor.Unit) =>
                clazz == classOf[java.util.function.LongConsumer]
              case (TypeConstructor.Int64, TypeConstructor.Bool) =>
                clazz == classOf[java.util.function.LongPredicate]
              case (TypeConstructor.Int64, TypeConstructor.Int64) =>
                clazz == classOf[java.util.function.LongUnaryOperator]
              case (TypeConstructor.Int64, TypeConstructor.Native(obj)) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.LongFunction[Object]]
              case (TypeConstructor.Native(obj), TypeConstructor.Unit) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.Consumer[Object]]
              case (TypeConstructor.Native(obj), TypeConstructor.Bool) if obj == classOf[Object] =>
                clazz == classOf[java.util.function.Predicate[Object]]
              case _ => false
            }
          case _ => false
        }
      // Null is a sub-type of every Java object and non-primitive Flix type
      case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Native(_), _)) => true
      case (Type.Cst(TypeConstructor.Null, _), tpe) if !isPrimitive(tpe) => true
      case _ => false
    }
  }

  /** Returns `true` if the given `tpe` is a primitive type. */
  private def isPrimitive(tpe: Type): Boolean = {
    tpe match {
      case Type.Cst(TypeConstructor.Bool, _) => true
      case Type.Cst(TypeConstructor.Char, _) => true
      case Type.Cst(TypeConstructor.Float32, _) => true
      case Type.Cst(TypeConstructor.Float64, _) => true
      case Type.Cst(TypeConstructor.Int8, _) => true
      case Type.Cst(TypeConstructor.Int16, _) => true
      case Type.Cst(TypeConstructor.Int32, _) => true
      case Type.Cst(TypeConstructor.Int64, _) => true
      case _ => false
    }
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

}
