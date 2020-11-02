package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{LocalResource, Options, StatUtils, Validation}
import ca.uwaterloo.flix.util.vt.TerminalContext

/**
  * A collection of internal utilities to measure the performance of the Flix compiler itself.
  */
object BenchmarkCompiler {

  /**
    * The number of compilations to perform before the statistics are collected.
    */
  val WarmupIterations = 25

  /**
    * The number of compilations to perform when collecting statistics.
    */
  val BenchmarkIterations = 10

  /**
    * Outputs statistics about time spent in each compiler phase.
    */
  def benchmarkPhases(o: Options): Unit = {
    val flix = newFlix(o)

    warmup(flix)

    flix.compile() match {
      case Validation.Success(compilationResult) =>
        // Check if we are in the last iteration.
        val currentTime = System.currentTimeMillis() / 1000
        val totalLines = compilationResult.getTotalLines()

        for (phase <- flix.phaseTimers) {
          val name = phase.phase
          val phaseTimeNanos = phase.time
          println(s"$name, $currentTime, $phaseTimeNanos")
        }
      case Validation.Failure(errors) =>
        errors.sortBy(_.source.name).foreach(e => println(e.message.fmt(TerminalContext.AnsiTerminal)))
        System.exit(1)
    }
  }

  /**
    * Computes the throughput of the compiler.
    */
  def benchmarkThroughput(o: Options): Unit = {
    val flix = newFlix(o)

    // Warmup
    warmup(flix)

    // Measure
    val results = (0 until BenchmarkIterations).map {
      case _ => flix.compile().get
    }

    // Computes the throughput in lines/sec.
    val totalLines = results.head.getTotalLines().toLong
    val totalTime = StatUtils.median(results.map(_.getTotalTime()).toList)
    val currentTime = System.currentTimeMillis() / 1000
    val throughput = (1_000_000_000L * totalLines) / totalTime // NB: Careful with loss of precision.

    println(s"$currentTime, $throughput")
  }

  /**
    * Runs the compiler a number of times to warmup the JIT.
    */
  private def warmup(flix: Flix): Unit = {
    for (i <- 0 until WarmupIterations) {
      flix.compile() match {
        case Validation.Success(compilationResult) => // nop
        case Validation.Failure(errors) =>
          errors.sortBy(_.source.name).foreach(e => println(e.message.fmt(TerminalContext.AnsiTerminal)))
          System.exit(1)
      }
    }
  }

  /**
    * Returns a Flix object configured with the benchmark program.
    */
  private def newFlix(o: Options): Flix = {
    val flix = new Flix()
    flix.setOptions(opts = o.copy(loadClassFiles = false, writeClassFiles = false))

    addCompilerTests(flix)
    addLibraryTests(flix)

    flix
  }

  /**
    * Adds the compiler test cases.
    */
  private def addCompilerTests(flix: Flix): Unit = {
    flix.addInput("Test.Eff.Polymorphism.flix", LocalResource.get("/test/flix/Test.Eff.Polymorphism.flix"))

    flix.addInput("Test.Exp.ArrayLength.flix", LocalResource.get("/test/flix/Test.Exp.ArrayLength.flix"))
    flix.addInput("Test.Exp.ArrayLit.flix", LocalResource.get("/test/flix/Test.Exp.ArrayLit.flix"))
    flix.addInput("Test.Exp.ArrayLoad.flix", LocalResource.get("/test/flix/Test.Exp.ArrayLoad.flix"))
    flix.addInput("Test.Exp.ArrayNew.flix", LocalResource.get("/test/flix/Test.Exp.ArrayNew.flix"))
    flix.addInput("Test.Exp.ArraySlice.flix", LocalResource.get("/test/flix/Test.Exp.ArraySlice.flix"))
    flix.addInput("Test.Exp.ArraySliceCopy.flix", LocalResource.get("/test/flix/Test.Exp.ArraySliceCopy.flix"))
    flix.addInput("Test.Exp.ArraySliceNoEndIndex.flix", LocalResource.get("/test/flix/Test.Exp.ArraySliceNoEndIndex.flix"))
    flix.addInput("Test.Exp.ArraySliceNoStartIndex.flix", LocalResource.get("/test/flix/Test.Exp.ArraySliceNoStartIndex.flix"))
    flix.addInput("Test.Exp.ArrayStore.flix", LocalResource.get("/test/flix/Test.Exp.ArrayStore.flix"))
    flix.addInput("Test.Exp.Ascribe.flix", LocalResource.get("/test/flix/Test.Exp.Ascribe.flix"))
    flix.addInput("Test.Exp.Binary.Spaceship.flix", LocalResource.get("/test/flix/Test.Exp.Binary.Spaceship.flix"))
    flix.addInput("Test.Exp.Cast.flix", LocalResource.get("/test/flix/Test.Exp.Cast.flix"))
    flix.addInput("Test.Exp.Choose.flix", LocalResource.get("/test/flix/Test.Exp.Choose.flix"))
    flix.addInput("Test.Exp.Concurrency.Buffered.flix", LocalResource.get("/test/flix/Test.Exp.Concurrency.Buffered.flix"))
    flix.addInput("Test.Exp.Concurrency.NewChannel.flix", LocalResource.get("/test/flix/Test.Exp.Concurrency.NewChannel.flix"))
    flix.addInput("Test.Exp.Concurrency.Unbuffered.flix", LocalResource.get("/test/flix/Test.Exp.Concurrency.Unbuffered.flix"))
    flix.addInput("Test.Exp.Concurrency.Spawn.flix", LocalResource.get("/test/flix/Test.Exp.Concurrency.Spawn.flix"))
    flix.addInput("Test.Exp.Hole.flix", LocalResource.get("/test/flix/Test.Exp.Hole.flix"))
    flix.addInput("Test.Exp.IfThenElse.flix", LocalResource.get("/test/flix/Test.Exp.IfThenElse.flix"))
    flix.addInput("Test.Exp.Infix.flix", LocalResource.get("/test/flix/Test.Exp.Infix.flix"))
    flix.addInput("Test.Exp.Interpolation.flix", LocalResource.get("/test/flix/Test.Exp.Interpolation.flix"))
    flix.addInput("Test.Exp.Jvm.GetField.flix", LocalResource.get("/test/flix/Test.Exp.Jvm.GetField.flix"))
    flix.addInput("Test.Exp.Jvm.GetStaticField.flix", LocalResource.get("/test/flix/Test.Exp.Jvm.GetStaticField.flix"))
    flix.addInput("Test.Exp.Jvm.InvokeConstructor.flix", LocalResource.get("/test/flix/Test.Exp.Jvm.InvokeConstructor.flix"))
    flix.addInput("Test.Exp.Jvm.InvokeMethod.flix", LocalResource.get("/test/flix/Test.Exp.Jvm.InvokeMethod.flix"))
    flix.addInput("Test.Exp.Jvm.InvokeStaticMethod.flix", LocalResource.get("/test/flix/Test.Exp.Jvm.InvokeStaticMethod.flix"))
    flix.addInput("Test.Exp.Jvm.PutField.flix", LocalResource.get("/test/flix/Test.Exp.Jvm.PutField.flix"))
    flix.addInput("Test.Exp.Jvm.PutStaticField.flix", LocalResource.get("/test/flix/Test.Exp.Jvm.PutStaticField.flix"))
    flix.addInput("Test.Exp.Let.MatchStar.flix", LocalResource.get("/test/flix/Test.Exp.Let.MatchStar.flix"))
    flix.addInput("Test.Exp.Match.Guard.flix", LocalResource.get("/test/flix/Test.Exp.Match.Guard.flix"))
    flix.addInput("Test.Exp.Match.Wild.flix", LocalResource.get("/test/flix/Test.Exp.Match.Wild.flix"))
    flix.addInput("Test.Exp.Reference.Assign.flix", LocalResource.get("/test/flix/Test.Exp.Reference.Assign.flix"))
    flix.addInput("Test.Exp.Reference.Deref.flix", LocalResource.get("/test/flix/Test.Exp.Reference.Deref.flix"))
    flix.addInput("Test.Exp.Reference.Precedence.flix", LocalResource.get("/test/flix/Test.Exp.Reference.Precedence.flix"))
    flix.addInput("Test.Exp.Reference.Ref.flix", LocalResource.get("/test/flix/Test.Exp.Reference.Ref.flix"))
    flix.addInput("Test.Exp.Null.flix", LocalResource.get("/test/flix/Test.Exp.Null.flix"))
    flix.addInput("Test.Exp.Postfix.flix", LocalResource.get("/test/flix/Test.Exp.Postfix.flix"))
    flix.addInput("Test.Exp.Record.Extend.flix", LocalResource.get("/test/flix/Test.Exp.Record.Extend.flix"))
    flix.addInput("Test.Exp.Record.Literal.flix", LocalResource.get("/test/flix/Test.Exp.Record.Literal.flix"))
    flix.addInput("Test.Exp.Record.Multiple.flix", LocalResource.get("/test/flix/Test.Exp.Record.Multiple.flix"))
    flix.addInput("Test.Exp.Record.Polymorphism.flix", LocalResource.get("/test/flix/Test.Exp.Record.Polymorphism.flix"))
    flix.addInput("Test.Exp.Record.Restrict.flix", LocalResource.get("/test/flix/Test.Exp.Record.Restrict.flix"))
    flix.addInput("Test.Exp.Record.Select.flix", LocalResource.get("/test/flix/Test.Exp.Record.Select.flix"))
    flix.addInput("Test.Exp.Record.Update.flix", LocalResource.get("/test/flix/Test.Exp.Record.Update.flix"))
    flix.addInput("Test.Exp.Stm.flix", LocalResource.get("/test/flix/Test.Exp.Stm.flix"))
    flix.addInput("Test.Exp.Tag.flix", LocalResource.get("/test/flix/Test.Exp.Tag.flix"))
    flix.addInput("Test.Exp.Tag.Lambda.flix", LocalResource.get("/test/flix/Test.Exp.Tag.Lambda.flix"))
    flix.addInput("Test.Exp.Unit.flix", LocalResource.get("/test/flix/Test.Exp.Unit.flix"))
    flix.addInput("Test.Predicate.Filter.flix", LocalResource.get("/test/flix/Test.Predicate.Filter.flix"))
    flix.addInput("Test.Predicate.Guard.flix", LocalResource.get("/test/flix/Test.Predicate.Guard.flix"))
    flix.addInput("Test.Predicate.Nullary.flix", LocalResource.get("/test/flix/Test.Predicate.Nullary.flix"))

    flix.addInput("Test.Use.Tag.flix", LocalResource.get("/test/flix/Test.Use.Tag.flix"))
  }

  /**
    * Adds the test cases for the standard library.
    */
  private def addLibraryTests(flix: Flix): Unit = {
    flix.addInput("TestBigInt.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestBigInt.flix"))
    flix.addInput("TestFloat32.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestFloat32.flix"))
    flix.addInput("TestFloat64.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestFloat64.flix"))
    flix.addInput("TestInt8.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt8.flix"))
    flix.addInput("TestInt16.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt16.flix"))
    flix.addInput("TestInt32.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt32.flix"))
    flix.addInput("TestInt64.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt64.flix"))
    flix.addInput("TestList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"))
    flix.addInput("TestMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"))
    flix.addInput("TestOption.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"))
    flix.addInput("TestPrelude.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestPrelude.flix"))
    flix.addInput("TestResult.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"))
    flix.addInput("TestSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"))
    flix.addInput("TestString.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestString.flix"))
  }

}
