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
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.util.Result.{Err, Ok}
import net.bytebuddy.ByteBuddy
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ClassDesc
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.util.Using

class TestFlix extends AnyFunSuite {

  test("close.ReleasesJars") {
    val jar = mkJar("closetest.A", "closetest.B")
    val flix = new Flix(jars = List(jar))
    try {
      flix.javaTypeProvider.lookupClass(ClassDesc.of("closetest.A")) match {
        case Ok(clazz) => assert(clazz.desc == ClassDesc.of("closetest.A"))
        case Err(error) => fail(error.toString)
      }
    } finally flix.close()

    // A class that was not looked up before the close can no longer be resolved or loaded.
    flix.javaTypeProvider.lookupClass(ClassDesc.of("closetest.B")) match {
      case Ok(_) => fail("Expected the JAR to be released.")
      case Err(_) => ()
    }
    assertThrows[ClassNotFoundException](flix.jarLoader.loadClass("closetest.B"))
  }

  test("close.Idempotent") {
    val flix = new Flix(jars = List(mkJar("closetest.A")))
    flix.close()
    flix.close()
  }

  /**
    * Writes a JAR file with one empty class per given binary `name` and returns its path.
    */
  private def mkJar(names: String*): Path = {
    val jar = Files.createTempFile("flix-test-", ".jar")
    Using.resource(new ZipOutputStream(Files.newOutputStream(jar))) { zip =>
      for (name <- names) {
        val bytes = new ByteBuddy().subclass(classOf[Object]).name(name).make().getBytes
        zip.putNextEntry(new ZipEntry(name.replace('.', '/') + ".class"))
        zip.write(bytes)
        zip.closeEntry()
      }
    }
    jar
  }

}
