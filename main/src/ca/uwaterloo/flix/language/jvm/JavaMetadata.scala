/*
 * Copyright 2026 Flix Authors
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
package ca.uwaterloo.flix.language.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.jvm.{JavaClass, JavaMethod}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_Object, CD_Throwable}

/**
  * Java class-file metadata queries for the phases that run after the Resolver.
  *
  * The Resolver reports a class whose metadata cannot be read as a compilation error, so a class that
  * reaches a later phase has already been read once. A failure here is therefore a compiler bug, and
  * every query throws an [[InternalCompilerException]] at the given location instead of returning a
  * `Result`.
  */
object JavaMetadata {

  /** Returns the class metadata of `desc`. */
  def lookupClass(desc: ClassDesc, loc: SourceLocation)(implicit flix: Flix): JavaClass =
    flix.javaTypeProvider.lookupClass(desc) match {
      case Ok(clazz) => clazz
      case Err(error) => throw InternalCompilerException(s"Java class lookup failed for '${ClassDescs.binaryNameOf(desc)}': $error", loc)
    }

  /**
    * Returns `true` if the Java type `sub` is a subtype of the Java type `sup`.
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
    * Returns the methods of `desc` that an anonymous subclass may override.
    *
    * See [[JavaMemberResolver.overridableMethods]] for which methods qualify.
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

}
