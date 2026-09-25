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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.errors.{ResolutionError, TypeError}
import ca.uwaterloo.flix.util.Options
import net.bytebuddy.ByteBuddy
import net.bytebuddy.description.modifier.{Ownership, Visibility}
import net.bytebuddy.implementation.StubMethod
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

/**
  * Tests the diagnostics for Java classes that cannot be read from the class path.
  *
  * The tests compile against a JAR that refers to a class it does not contain, as happens when a
  * dependency of a user JAR is not on the class path.
  */
class TestJavaClassPath extends AnyFunSuite with TestUtils {

  /** The class that the JAR refers to but does not contain. */
  private val Missing = "dev.flix.classpath.Missing"

  /**
    * A JAR with the following classes, generated once for the suite. `Missing` is left out of it.
    *
    *   - `class SubclassOfMissing extends Missing`
    *   - `class Factory { static Missing make(); static int accept(Missing m); Missing get(); Missing field; static Missing FIELD; }`
    *   - `interface Producer { Missing produce(); }`
    */
  private lazy val jar: Path = {
    val missing = new ByteBuddy().subclass(classOf[Object]).name(Missing).make()
    val missingType = missing.getTypeDescription

    val subclassOfMissing = new ByteBuddy().subclass(missingType).name("dev.flix.classpath.SubclassOfMissing").make()

    val factory = new ByteBuddy().subclass(classOf[Object]).name("dev.flix.classpath.Factory")
      .defineMethod("make", missingType, Visibility.PUBLIC, Ownership.STATIC).intercept(StubMethod.INSTANCE)
      .defineMethod("accept", classOf[Int], Visibility.PUBLIC, Ownership.STATIC).withParameters(missingType).intercept(StubMethod.INSTANCE)
      .defineMethod("get", missingType, Visibility.PUBLIC).intercept(StubMethod.INSTANCE)
      .defineField("field", missingType, Visibility.PUBLIC)
      .defineField("FIELD", missingType, Visibility.PUBLIC, Ownership.STATIC)
      .make()

    val producer = new ByteBuddy().makeInterface().name("dev.flix.classpath.Producer")
      .defineMethod("produce", missingType, Visibility.PUBLIC).withoutCode()
      .make()

    val path = Files.createTempFile("flix-test-classpath", ".jar")
    path.toFile.deleteOnExit()
    subclassOfMissing.include(factory, producer).toJar(path.toFile)
    path
  }

  /** Compiles `input` against the JAR. */
  private def checkWithJar(input: String): (Option[TypedAst.Root], List[CompilationMessage]) =
    new Flix().setOptions(Options.TestWithLibNix).addJar(jar).addVirtualPath(CompilerConstants.VirtualTestFile, input).check()

  test("UnreadableJvmClass.Import.MissingSuperclass") {
    val input =
      """
        |import dev.flix.classpath.SubclassOfMissing
        |
        |def f(): Unit \ IO = { let _ = new SubclassOfMissing(); () }
      """.stripMargin
    expectError[ResolutionError.UnreadableJvmClass](checkWithJar(input))
  }

  test("UnreadableJvmClass.StaticField.MissingType") {
    val input =
      """
        |import dev.flix.classpath.Factory
        |
        |def f(): Unit \ IO = { let _ = Factory.FIELD; () }
      """.stripMargin
    expectError[ResolutionError.UnreadableJvmClass](checkWithJar(input))
  }

  test("UnreadableJvmClass.NewObject.MissingMethodType") {
    val input =
      """
        |import dev.flix.classpath.Producer
        |
        |def f(): Producer \ IO = new Producer { def produce(_this: Producer): Int32 = 1 }
      """.stripMargin
    expectError[ResolutionError.UnreadableJvmClass](checkWithJar(input))
  }

  test("UnreadableJvmClass.StaticMethod.MissingReturnType") {
    val input =
      """
        |import dev.flix.classpath.Factory
        |
        |def f(): Unit \ IO = { let _ = Factory.make(); () }
      """.stripMargin
    expectError[TypeError.UnreadableJvmClass](checkWithJar(input))
  }

  test("UnreadableJvmClass.StaticMethod.MissingParameterType") {
    val input =
      """
        |import dev.flix.classpath.Factory
        |
        |def f(): Int32 \ IO = Factory.accept("hello")
      """.stripMargin
    expectError[TypeError.UnreadableJvmClass](checkWithJar(input))
  }

  test("UnreadableJvmClass.Method.MissingReturnType") {
    val input =
      """
        |import dev.flix.classpath.Factory
        |
        |def f(): Unit \ IO = { let _ = new Factory().get(); () }
      """.stripMargin
    expectError[TypeError.UnreadableJvmClass](checkWithJar(input))
  }

  test("UnreadableJvmClass.Field.MissingType") {
    val input =
      """
        |import dev.flix.classpath.Factory
        |
        |def f(): Unit \ IO = { let factory = new Factory(); let _ = factory.field; () }
      """.stripMargin
    expectError[TypeError.UnreadableJvmClass](checkWithJar(input))
  }

}
