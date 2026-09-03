/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.typer.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.jvm.{JavaClass, JavaMethod, JavaType, JavaTypeVariable}
import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.language.jvm.{ClassDescs, JavaClasses, JavaHierarchy, JavaMemberResolver}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.*

/**
  * Builds Flix types from Java class descriptors using the class-file metadata of the Java type provider.
  *
  * No class is ever loaded: the number of type parameters of a class is read from its metadata.
  */
object JavaTypes {

  /** Returns the class metadata of `desc`, or throws an [[InternalCompilerException]] if it cannot be read. */
  def lookupClass(desc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): JavaClass =
    flix.javaTypeProvider.lookupClass(desc) match {
      case Ok(clazz) => clazz
      case Err(error) => throw InternalCompilerException(s"Java class lookup failed for '${ClassDescs.binaryNameOf(desc)}': $error", loc)
    }

  /**
    * Returns the number of type parameters of the class `desc`.
    *
    * Primitive and array types have no type parameters.
    */
  def typeParameterCount(desc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): Int =
    if (desc.isClassOrInterface) lookupClass(desc, loc).typeParameters.length else 0

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
    * A type variable that is not in `subst`, a generic array, and a wildcard fall back to `Object`.
    */
  def flixTypeOf(javaType: JavaType, subst: Map[JavaTypeVariable, Type], loc: SourceLocation)(implicit flix: Flix): Type = javaType match {
    case JavaType.Variable(variable, _) =>
      subst.getOrElse(variable, Type.mkObject(loc))
    case JavaType.Parameterized(erasure, arguments) =>
      val base = flixTypeOf(erasure, loc)
      val resolvedArgs = arguments.map(flixTypeOf(_, subst, loc))
      Type.mkApply(base, resolvedArgs, loc)
    case JavaType.NonGeneric(erasure) =>
      instantiateWithObjectArgs(erasure, loc)
    case JavaType.GenericArray(_, _) =>
      Type.mkObject(loc)
    case JavaType.Wildcard(_, _, _) =>
      Type.mkObject(loc)
  }

  /**
    * Returns `true` if the Java type `sub` is a subtype of the Java type `sup`, or throws an
    * [[InternalCompilerException]] if their metadata cannot be read.
    *
    * See [[JavaHierarchy.isSubtype]] for the subtyping rules.
    */
  def isSubtype(sub: ClassDesc, sup: ClassDesc, loc: SourceLocation)(implicit flix: Flix): Boolean =
    JavaHierarchy.isSubtype(sub, sup) match {
      case Ok(result) => result
      case Err(error) => throw InternalCompilerException(s"Java subtype check failed for '${ClassDescs.binaryNameOf(sub)} <: ${ClassDescs.binaryNameOf(sup)}': $error", loc)
    }

  /** Returns `true` if `desc` is `java.lang.Throwable` or a subclass of it. */
  def isThrowable(desc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): Boolean =
    isSubtype(desc, CD_Throwable, loc)

  /**
    * Returns the methods of `desc` that an anonymous subclass may override, or throws an
    * [[InternalCompilerException]] if its metadata cannot be read.
    */
  def overridableMethods(desc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): List[JavaMethod] =
    JavaMemberResolver.overridableMethods(desc) match {
      case Ok(methods) => methods
      case Err(error) => throw InternalCompilerException(s"Java method lookup failed for '${ClassDescs.binaryNameOf(desc)}': $error", loc)
    }

  /** Returns `true` if `method` is or overrides a method declared by `java.lang.Object`. */
  def isObjectMethod(method: JavaMethod, loc: SourceLocation)(implicit flix: Flix): Boolean =
    lookupClass(CD_Object, loc).declaredMethods.exists { m =>
      m.ref.name == method.ref.name && m.ref.descriptor.parameterList() == method.ref.descriptor.parameterList()
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

}
