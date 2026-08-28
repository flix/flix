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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.jvm.JavaFieldRef
import ca.uwaterloo.flix.language.phase.typer.jvm.{ByteBuddyJavaTypeProvider, JavaMemberResolver}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{ClassDescs, InternalCompilerException}

import java.lang.ref.WeakReference
import java.lang.reflect.Field
import java.util.WeakHashMap

/** Checks descriptor-based Java field resolution against reflective type reduction. */
private[typer] object JavaTypeReductionShadow {

  private val resolvers = new WeakHashMap[ClassLoader, WeakReference[JavaMemberResolver]]()

  /** Compares descriptor-based instance-field resolution with the authoritative reflective result. */
  def compareField(owner: Class[?], name: String, reflective: Option[Field], loc: SourceLocation): Unit = {
    val descriptorOwner = ClassDescs.of(owner)
    val query = s"${descriptorOwner.displayName()}.$name"
    resolverFor(owner).field(descriptorOwner, name, static = false) match {
      case Err(error) =>
        throw InternalCompilerException(s"Java field shadow lookup failed for '$query': $error", loc)
      case Ok(descriptor) =>
        val reflectiveRef = reflective.map(toRef)
        val descriptorRef = descriptor.map(_.ref)
        if (reflectiveRef != descriptorRef) {
          throw InternalCompilerException(
            s"Java field lookup mismatch for '$query': reflection=$reflectiveRef, descriptor=$descriptorRef",
            loc
          )
        }
    }
  }

  /** Returns the cached descriptor resolver for the class loader of `owner`. */
  private def resolverFor(owner: Class[?]): JavaMemberResolver = resolvers.synchronized {
    val loader = owner.getClassLoader
    Option(resolvers.get(loader)).flatMap(ref => Option(ref.get())).getOrElse {
      val resolver = JavaMemberResolver(ByteBuddyJavaTypeProvider.fromClassLoader(loader))
      resolvers.put(loader, new WeakReference(resolver))
      resolver
    }
  }

  /** Returns the descriptor-based reference for the reflective `field`. */
  private def toRef(field: Field): JavaFieldRef =
    JavaFieldRef(ClassDescs.of(field.getDeclaringClass), field.getName, ClassDescs.of(field.getType))

}
