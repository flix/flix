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
package ca.uwaterloo.flix.language.phase.typer.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.jvm.JavaField
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.Ok

import java.lang.constant.ClassDesc
import java.lang.reflect.Modifier

/** Resolves accessible Java members using descriptor-based class-file metadata. */
object JavaMemberResolver {

  /** Returns `Ok` with the selected public field, or `Err` if class metadata cannot be read. */
  def field(owner: ClassDesc, name: String, static: Boolean)(implicit flix: Flix): Result[Option[JavaField], JavaLookupError] =
    findField(owner, name, Set.empty).map(_.filter(f => Modifier.isStatic(f.modifiers) == static))

  /** Returns `Ok` with the first public field selected from `owner`, or `Err` if class metadata cannot be read. */
  private def findField(owner: ClassDesc, name: String, visited: Set[ClassDesc])(implicit flix: Flix): Result[Option[JavaField], JavaLookupError] = {
    if (visited.contains(owner)) {
      Ok(None)
    } else {
      flix.javaTypeProvider.lookupClass(owner).flatMap { clazz =>
        clazz.declaredFields.find(f => f.ref.name == name && Modifier.isPublic(f.modifiers)) match {
          case Some(field) => Ok(Some(field))
          case None =>
            findFieldIn(clazz.interfaces.map(_.erasure), name, visited + owner).flatMap {
              case Some(field) => Ok(Some(field))
              case None => clazz.superClass match {
                case None => Ok(None)
                case Some(parent) => findField(parent.erasure, name, visited + owner)
              }
            }
        }
      }
    }
  }

  /** Returns `Ok` with the first public field selected from `owners`, or `Err` if class metadata cannot be read. */
  private def findFieldIn(owners: List[ClassDesc], name: String, visited: Set[ClassDesc])(implicit flix: Flix): Result[Option[JavaField], JavaLookupError] = owners match {
    case Nil => Ok(None)
    case owner :: rest => findField(owner, name, visited).flatMap {
      case Some(field) => Ok(Some(field))
      case None => findFieldIn(rest, name, visited)
    }
  }

}
