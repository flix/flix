/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{LocalResource, Options}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

object CompilerMemory {

  /**
    * Crudely measure compiler memory usage.
    */
  def run(o: Options): Unit = {
    // Run the Flix compiler on some input.
    val flix = new Flix
    addInputs(flix)
    val result = flix.compile()

    sleepAndGc()
    measureMemoryUsage(o)
  }

  /**
    * Sleeps a bit and hints the GC to run.
    */
  private def sleepAndGc(): Unit = {
    for (i <- 0 until 5) {
      Thread.sleep(1_000)
      System.gc()
    }
  }

  /**
    * Prints the estimated amount of memory used (in megabytes).
    */
  private def measureMemoryUsage(o: Options): Unit = {
    val usedMemory = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    if (o.json) {
      val json = ("bytes" -> usedMemory)
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println(s"Used Memory: ${usedMemory / (1_024 * 1_024)} MB")
    }
  }

  /**
    * Adds test code to the benchmark.
    */
  private def addInputs(flix: Flix): Unit = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    flix.addSourceCode("TestArray.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestArray.flix"))
    flix.addSourceCode("TestIterator.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestIterator.flix"))
    flix.addSourceCode("TestList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"))
    flix.addSourceCode("TestMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"))
    flix.addSourceCode("TestMutDeque.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutDeque.flix"))
    flix.addSourceCode("TestMutList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutList.flix"))
    flix.addSourceCode("TestNel.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestNel.flix"))
    flix.addSourceCode("TestOption.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"))
    flix.addSourceCode("TestResult.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"))
    flix.addSourceCode("TestSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"))
    flix.addSourceCode("TestValidation.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestValidation.flix"))
  }

}
