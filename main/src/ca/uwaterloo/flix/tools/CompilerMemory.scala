/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{LocalResource, Options}
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods

import java.nio.file.Path

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
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addVirtualPath(Path.of("TestArray.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestArray.flix"))
    flix.addVirtualPath(Path.of("TestIterator.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestIterator.flix"))
    flix.addVirtualPath(Path.of("TestList.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"))
    flix.addVirtualPath(Path.of("TestMap.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"))
    flix.addVirtualPath(Path.of("TestMutDeque.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutDeque.flix"))
    flix.addVirtualPath(Path.of("TestMutList.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutList.flix"))
    flix.addVirtualPath(Path.of("TestNel.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestNel.flix"))
    flix.addVirtualPath(Path.of("TestOption.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"))
    flix.addVirtualPath(Path.of("TestResult.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"))
    flix.addVirtualPath(Path.of("TestSet.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"))
    flix.addVirtualPath(Path.of("TestValidation.flix"), LocalResource.get("/test/ca/uwaterloo/flix/library/TestValidation.flix"))
  }

}
