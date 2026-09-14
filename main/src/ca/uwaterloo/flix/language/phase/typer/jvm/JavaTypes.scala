/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.typer.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.jvm.{JavaType, JavaTypeVariable}
import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.language.jvm.{JavaClasses, JavaMetadata}

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.*

/**
  * Builds Flix types from Java class descriptors using the class-file metadata of the Java type provider.
  *
  * No class is ever loaded: the number of type parameters of a class is read from its metadata.
  */
object JavaTypes {

  /**
    * Returns the number of type parameters of the class `desc`.
    *
    * Primitive and array types have no type parameters.
    */
  private def typeParameterCount(desc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): Int =
    if (desc.isClassOrInterface) JavaMetadata.lookupClass(desc, loc).typeParameters.length else 0

  /**
    * Returns the Flix type of the Java class `desc`.
    *
    * Primitive types, `String`, `BigInteger`, `BigDecimal`, and `Pattern` map to their Flix counterparts,
    * arrays map to Flix arrays, and any other class maps to a native type whose arity is the number of
    * type parameters of the class (or of the element class of an array).
    */
  def flixTypeOf(desc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): Type =
    flixTypeOf(desc, typeParameterCount(elementTypeOf(desc), loc))

  /**
    * Returns the Flix type of the Java class `desc` whose element class has `arity` type parameters.
    *
    * Arrays are returned with the [[Type.IO]] region. Since an array class has no type parameters
    * of its own, `arity` is the number of type parameters of its (innermost) element class.
    *
    * Returns a [[TypeConstructor.Native]] of `desc` if nothing more specific is found. The `arity`
    * is only evaluated in that case, so callers may compute it lazily.
    */
  def flixTypeOf(desc: ClassDesc, arity: => Int): Type = desc match {
    case CD_boolean => Type.Bool
    case CD_byte => Type.Int8
    case CD_short => Type.Int16
    case CD_int => Type.Int32
    case CD_long => Type.Int64
    case CD_char => Type.Char
    case CD_float => Type.Float32
    case CD_double => Type.Float64
    case CD_void => Type.Unit
    case CD_String => Type.Str
    case JavaClasses.BigDecimal => Type.BigDecimal
    case JavaClasses.BigInteger => Type.BigInt
    case JavaClasses.Regex => Type.Regex
    case _ if desc.isArray =>
      val elmType = flixTypeOf(desc.componentType(), arity)
      Type.mkArray(elmType, Type.IO, SourceLocation.Unknown)
    case _ => Type.mkNative(desc, arity, SourceLocation.Unknown)
  }

  /**
    * Returns the descriptor of the Java class of `tpe`, if it exists.
    *
    * Almost the inverse of `flixTypeOf(desc, arity)`, but arrays and `Unit` return `None`.
    */
  def descriptorOf(tpe: Type): Option[ClassDesc] = tpe match {
    case Type.Bool => Some(CD_boolean)
    case Type.Int8 => Some(CD_byte)
    case Type.Int16 => Some(CD_short)
    case Type.Int32 => Some(CD_int)
    case Type.Int64 => Some(CD_long)
    case Type.Char => Some(CD_char)
    case Type.Float32 => Some(CD_float)
    case Type.Float64 => Some(CD_double)
    case Type.Cst(TypeConstructor.BigDecimal, _) => Some(JavaClasses.BigDecimal)
    case Type.Cst(TypeConstructor.BigInt, _) => Some(JavaClasses.BigInteger)
    case Type.Cst(TypeConstructor.Str, _) => Some(CD_String)
    case Type.Cst(TypeConstructor.Regex, _) => Some(JavaClasses.Regex)
    case Type.Cst(TypeConstructor.Native(desc, _), _) => Some(desc)
    case _ =>
      // Peel off type applications (e.g., ArrayList[String]) and check the base type.
      tpe.baseType match {
        case Type.Cst(TypeConstructor.Native(desc, _), _) => Some(desc)
        case _ => None
      }
  }

  /**
    * Returns the erased Java class descriptor of the non-null Flix type `tpe`, as used for member lookup.
    *
    * Types with a Java counterpart (see [[descriptorOf]]) erase to it. Arrays and vectors erase to Java
    * arrays, functions to their Java functional interfaces (see [[lookupFunIF]]), and every other type,
    * including type variables, to `Object`.
    */
  def erasedDescriptorOf(tpe: Type): ClassDesc = descriptorOf(tpe).getOrElse(tpe match {
    // Arrays and vectors erase to Java arrays. A null element type falls back to Object.
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Array, _), elmType, _), _, _) =>
      erasedArrayDescriptorOf(elmType)
    case Type.Apply(Type.Cst(TypeConstructor.Vector, _), elmType, _) =>
      erasedArrayDescriptorOf(elmType)

    // Functions map to the same Java functional interfaces as the reflective path.
    case Type.Apply(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Arrow(2), _), _, _), varArg, _), varRet, _) =>
      lookupFunIF(varArg, varRet).map(_.desc).getOrElse(CD_Object)
    case _ => CD_Object
  })

  /** Returns the Java array descriptor for an array or vector element type. */
  private def erasedArrayDescriptorOf(elmType: Type): ClassDesc = elmType match {
    case Type.Cst(TypeConstructor.Null, _) => CD_Object.arrayType()
    case _ => erasedDescriptorOf(elmType).arrayType()
  }

  /**
    * Returns `true` if an argument of type `tpe` for the varargs parameter `paramDesc` is the varargs array
    * itself, and `false` if it is a single element that must be wrapped in an array.
    *
    * The decision mirrors the applicability check of [[ca.uwaterloo.flix.language.jvm.JavaMemberResolver]]:
    * an argument that is assignable to the array parameter is passed directly, and only an argument that is
    * not assignable is expanded. `null` is assignable to every array type and is passed as the array.
    *
    *   - `Vector[String]` for `String...` is the array.
    *   - `String` for `String...` is an element.
    *   - `Vector[Int32]` for `T...`, which erases to `Object[]`, is an element since `int[]` is not an `Object[]`.
    */
  def isVarArgsArray(tpe: Type, paramDesc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): Boolean = tpe match {
    case Type.Cst(TypeConstructor.Null, _) => true
    case _ => JavaMetadata.isSubtype(erasedDescriptorOf(tpe), paramDesc, loc)
  }

  /**
    * Returns the string representation of the Java type `desc` used in error messages: a primitive,
    * an array, or a class with a Flix counterpart (e.g. `String` or `BigInt`) is shown as its Flix
    * type, and any other class by its binary name.
    */
  def formatType(desc: ClassDesc): String =
    flixTypeOf(desc, 0).toString

  /**
    * Returns the fully-applied Flix type of the Java class `desc`, with `Object` type arguments for a generic class.
    *
    * Use this in ground-type contexts that need kind `Star`.
    */
  def instantiateWithObjectArgs(desc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): Type =
    instantiate(desc, loc)(Type.mkObject(loc))

  /** Like [[instantiateWithObjectArgs]] but uses fresh type variables instead of `Object`. */
  def instantiateWithFreshVars(desc: ClassDesc, scope: RegionScope, loc: SourceLocation)(implicit flix: Flix): Type =
    instantiate(desc, loc)(Type.freshVar(Kind.Star, loc)(scope, flix))

  /**
    * Returns the Flix type of the Java type `javaType` under the substitution `subst` of type variables.
    *
    * `fallback` is evaluated once for every position that has no Flix counterpart: a type variable that
    * is not in `subst`, a wildcard whose upper bound is not a type variable, and each type argument of a
    * raw generic class. Pass `Object` in ground-type contexts and a fresh type variable where the result
    * takes part in unification.
    *
    *   - A wildcard `? extends T` resolves to the resolution of `T`.
    *   - A generic array `T[]` resolves to a Flix array of the resolution of `T` in the `IO` region.
    */
  def flixTypeOf(javaType: JavaType, subst: Map[JavaTypeVariable, Type], loc: SourceLocation)(fallback: => Type)(implicit flix: Flix): Type = javaType match {
    case JavaType.Variable(variable, _) =>
      subst.getOrElse(variable, fallback)
    case JavaType.Parameterized(erasure, arguments) =>
      val base = flixTypeOf(erasure, loc)
      val resolvedArgs = arguments.map(flixTypeOf(_, subst, loc)(fallback))
      Type.mkApply(base, resolvedArgs, loc)
    case JavaType.NonGeneric(erasure) =>
      instantiate(erasure, loc)(fallback)
    case JavaType.GenericArray(component, _) =>
      Type.mkArray(flixTypeOf(component, subst, loc)(fallback), Type.IO, loc)
    case JavaType.Wildcard(upperBounds, _, _) =>
      upperBounds match {
        case (variable: JavaType.Variable) :: _ => flixTypeOf(variable, subst, loc)(fallback)
        case _ => fallback
      }
  }

  /** Applies the Flix type of `desc` to one `mkArg` per type parameter of `desc`. */
  private def instantiate(desc: ClassDesc, loc: SourceLocation)(mkArg: => Type)(implicit flix: Flix): Type =
    flixTypeOf(desc, loc) match {
      case base@Type.Cst(TypeConstructor.Native(_, arity), _) => Type.mkApply(base, List.fill(arity)(mkArg), loc)
      case base => base // Primitive types, arrays, and the special classes have no type parameters.
    }

  /** Returns the innermost element type of the array `desc`, or `desc` itself if it is not an array. */
  private def elementTypeOf(desc: ClassDesc): ClassDesc =
    if (desc.isArray) elementTypeOf(desc.componentType()) else desc

  /**
    * Maps a Flix Arrow type to its Java functional interface.
    * `argParam`/`retParam` name the interface type param that corresponds
    * to the Arrow's argument/return type (None for primitive-specialized
    * interfaces like IntConsumer that have no type params).
    */
  case class FunIFMapping(
    desc: ClassDesc,
    argParam: Option[String],
    retParam: Option[String]
  )

  /** Looks up the Java functional interface for a Flix Arrow with the given arg and ret types. */
  def lookupFunIF(argType: Type, retType: Type): Option[FunIFMapping] = {
    import TypeConstructor.*
    (argType, retType) match {
      case (Type.Cst(Int32, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(JavaClasses.IntConsumer, None, None))
      case (Type.Cst(Int32, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(JavaClasses.IntPredicate, None, None))
      case (Type.Cst(Int32, _), Type.Cst(Int32, _)) =>
        Some(FunIFMapping(JavaClasses.IntUnaryOperator, None, None))
      case (Type.Cst(Int32, _), _) =>
        Some(FunIFMapping(JavaClasses.IntFunction, None, Some("R")))
      case (Type.Cst(Int64, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(JavaClasses.LongConsumer, None, None))
      case (Type.Cst(Int64, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(JavaClasses.LongPredicate, None, None))
      case (Type.Cst(Int64, _), Type.Cst(Int64, _)) =>
        Some(FunIFMapping(JavaClasses.LongUnaryOperator, None, None))
      case (Type.Cst(Int64, _), _) =>
        Some(FunIFMapping(JavaClasses.LongFunction, None, Some("R")))
      case (Type.Cst(Float64, _), Type.Cst(Unit, _)) =>
        Some(FunIFMapping(JavaClasses.DoubleConsumer, None, None))
      case (Type.Cst(Float64, _), Type.Cst(Bool, _)) =>
        Some(FunIFMapping(JavaClasses.DoublePredicate, None, None))
      case (Type.Cst(Float64, _), Type.Cst(Float64, _)) =>
        Some(FunIFMapping(JavaClasses.DoubleUnaryOperator, None, None))
      case (Type.Cst(Float64, _), _) =>
        Some(FunIFMapping(JavaClasses.DoubleFunction, None, Some("R")))
      case (_, Type.Cst(Unit, _)) =>
        Some(FunIFMapping(JavaClasses.ObjConsumer, Some("T"), None))
      case (_, Type.Cst(Bool, _)) =>
        Some(FunIFMapping(JavaClasses.ObjPredicate, Some("T"), None))
      case (_, _) =>
        Some(FunIFMapping(JavaClasses.ObjFunction, Some("T"), Some("R")))
    }
  }

}
