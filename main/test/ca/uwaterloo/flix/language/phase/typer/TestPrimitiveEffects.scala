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

import ca.uwaterloo.flix.util.{ClassDescs, LocalResource}
import org.json4s.JsonAST.*
import org.json4s.jvalue2monadic
import org.json4s.native.JsonMethods.parse
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ClassDesc

/**
  * Checks that the primitive effect tables name classes and methods that actually exist.
  *
  * [[PrimitiveEffects]] parses the tables into descriptors without loading any class, so a name that no longer
  * resolves would silently become an unreachable key. These tests reflect over the JDK to catch that.
  */
class TestPrimitiveEffects extends AnyFunSuite {

  /** The path to the package effects. */
  private val PackageEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/PrimitiveEffects.Packages.json"

  /** The path to the class effects. */
  private val ClassEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/PrimitiveEffects.Classes.json"

  /** The path to the constructor effects. */
  private val ConstructorEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/PrimitiveEffects.Constructors.json"

  /** The path to the method effects. */
  private val MethodEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/PrimitiveEffects.Methods.json"

  /** A dot-separated sequence of Java identifiers, i.e. what `ClassDesc.packageName` returns. */
  private val PackageNameRegex = """[A-Za-z_$][A-Za-z0-9_$]*(\.[A-Za-z_$][A-Za-z0-9_$]*)*""".r

  test("Packages.PackageNames") {
    val packageNames = keysOf(PackageEffsPath, "packages")
    assert(packageNames.nonEmpty)
    for (packageName <- packageNames) {
      assert(PackageNameRegex.matches(packageName), s"'$packageName' is not a package name.")
    }
  }

  test("Classes.ClassNames") {
    val classNames = keysOf(ClassEffsPath, "classes")
    assert(classNames.nonEmpty)
    classNames.foreach(checkClassName)
  }

  test("Constructors.ClassNames") {
    val classNames = keysOf(ConstructorEffsPath, "constructors")
    assert(classNames.nonEmpty)
    classNames.foreach(checkClassName)
  }

  test("Methods.DeclaringClasses") {
    val keys = keysOf(MethodEffsPath, "methods")
    assert(keys.nonEmpty)
    for (key <- keys) {
      val cc = key.indexOf("::")
      assert(cc > 0, s"'$key' is not of the form 'className::methodName'.")
      val className = key.substring(0, cc)
      val methodName = key.substring(cc + 2)
      val clazz = checkClassName(className)

      val methods = clazz.getMethods.filter(_.getName == methodName)
      assert(methods.nonEmpty, s"The class '$className' has no public method named '$methodName'.")
      for (method <- methods) {
        // A resolved JavaMethod refers to its declaring class, so the table must name that class.
        val declaringClassName = method.getDeclaringClass.getName
        assert(declaringClassName == className, s"'$key' must name the declaring class '$declaringClassName'.")
      }
    }
  }

  /** Returns the class named `className`, asserting that its descriptor is the one the tables are keyed by. */
  private def checkClassName(className: String): Class[?] = {
    val clazz = try {
      Class.forName(className)
    } catch {
      case _: ClassNotFoundException => fail(s"The class '$className' does not exist.")
    }
    assert(ClassDesc.of(className) == ClassDescs.of(clazz), s"'$className' is not the binary name of the class.")
    clazz
  }

  /** Returns the keys of the object `field` of the JSON resource at `path`. */
  private def keysOf(path: String, field: String): List[String] = {
    parse(LocalResource.get(path)) \\ field match {
      case JObject(l) => l.map { case (key, _) => key }
      case _ => fail(s"Unexpected JSON format in '$path'.")
    }
  }

}
