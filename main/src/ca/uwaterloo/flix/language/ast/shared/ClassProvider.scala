/*
 * Copyright 2025 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

/**
  * Provides lazy access to parsed Java class models.
  *
  * Implementations discover class files from a source (e.g. the Java runtime
  * image or a JAR file) and parse them on demand.
  *
  * The return type is `Any` rather than `java.lang.classfile.ClassModel` because
  * Scala 2.13 cannot resolve the sealed interface hierarchy introduced in JDK 24.
  * Callers should cast via `result.asInstanceOf[java.lang.classfile.ClassModel]`.
  */
trait ClassProvider {

  /**
    * Returns a parsed class model for the given internal name
    * (e.g., `"java/util/List"`), or `None` if the class is not known.
    *
    * The returned value is a `java.lang.classfile.ClassModel` stored as `Any`.
    *
    * Implementations should cache the result so that repeated lookups
    * do not re-parse.
    */
  def getClassModel(internalName: String): Option[Any]

  /**
    * Returns all known internal class names. This is cheap — no parsing occurs.
    */
  def classNames: Iterable[String]

}

object ClassProvider {

  /**
    * An empty `ClassProvider` that knows no classes.
    */
  val empty: ClassProvider = new ClassProvider {
    def getClassModel(internalName: String): Option[Any] = None
    def classNames: Iterable[String] = Iterable.empty
  }

}
