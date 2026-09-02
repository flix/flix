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
import ca.uwaterloo.flix.language.ast.jvm.JavaClass
import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.lang.constant.ClassDesc

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
      case Err(error) => throw InternalCompilerException(s"Java class lookup failed for '${desc.displayName()}': $error", loc)
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
    Type.getFlixType(desc, typeParameterCount(elementTypeOf(desc), loc))

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
