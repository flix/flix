package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{StatUtils, Validation}
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
  def benchmarkPhases(): Unit = {
    val flix = newFlix()

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
  def benchmarkThroughput(): Unit = {
    val flix = newFlix()

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
  private def newFlix(): Flix = {
    val flix = new Flix()
    flix.setOptions(opts = flix.options.copy(loadClassFiles = false, writeClassFiles = false))

    addCompilerTests(flix)
    addLibraryTests(flix)
    //addAbstractDomains(flix)

    flix
  }

  /**
    * Adds the compiler test cases.
    */
  private def addCompilerTests(flix: Flix): Unit = {
    flix.addPath("main/test/flix/Test.Eff.Polymorphism.flix")

    flix.addPath("main/test/flix/Test.Exp.ArrayLength.flix")
    flix.addPath("main/test/flix/Test.Exp.ArrayLit.flix")
    flix.addPath("main/test/flix/Test.Exp.ArrayLoad.flix")
    flix.addPath("main/test/flix/Test.Exp.ArrayNew.flix")
    flix.addPath("main/test/flix/Test.Exp.ArraySlice.flix")
    flix.addPath("main/test/flix/Test.Exp.ArraySliceCopy.flix")
    flix.addPath("main/test/flix/Test.Exp.ArraySliceNoEndIndex.flix")
    flix.addPath("main/test/flix/Test.Exp.ArraySliceNoStartIndex.flix")
    flix.addPath("main/test/flix/Test.Exp.ArrayStore.flix")
    flix.addPath("main/test/flix/Test.Exp.Ascribe.flix")
    flix.addPath("main/test/flix/Test.Exp.Binary.Spaceship.flix")
    flix.addPath("main/test/flix/Test.Exp.Cast.flix")
    flix.addPath("main/test/flix/Test.Exp.Concurrency.Buffered.flix")
    flix.addPath("main/test/flix/Test.Exp.Concurrency.NewChannel.flix")
    flix.addPath("main/test/flix/Test.Exp.Concurrency.Unbuffered.flix")
    flix.addPath("main/test/flix/Test.Exp.Concurrency.Spawn.flix")
    flix.addPath("main/test/flix/Test.Exp.Jvm.GetField.flix")
    flix.addPath("main/test/flix/Test.Exp.Jvm.GetStaticField.flix")
    flix.addPath("main/test/flix/Test.Exp.Jvm.InvokeConstructor.flix")
    flix.addPath("main/test/flix/Test.Exp.Jvm.InvokeMethod.flix")
    flix.addPath("main/test/flix/Test.Exp.Jvm.InvokeStaticMethod.flix")
    flix.addPath("main/test/flix/Test.Exp.Jvm.PutField.flix")
    flix.addPath("main/test/flix/Test.Exp.Jvm.PutStaticField.flix")
    flix.addPath("main/test/flix/Test.Exp.Let.MatchStar.flix")
    flix.addPath("main/test/flix/Test.Exp.Reference.Assign.flix")
    flix.addPath("main/test/flix/Test.Exp.Reference.Deref.flix")
    flix.addPath("main/test/flix/Test.Exp.Reference.Precedence.flix")
    flix.addPath("main/test/flix/Test.Exp.Reference.Ref.flix")
    flix.addPath("main/test/flix/Test.Exp.Record.Polymorphism.flix")
    flix.addPath("main/test/flix/Test.Exp.Stm.flix")
    flix.addPath("main/test/flix/Test.Exp.Tag.flix")
    flix.addPath("main/test/flix/Test.Exp.Tag.Lambda.flix")
    flix.addPath("main/test/flix/Test.Use.Tag.flix")


    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Arithmetic.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Bitwise.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Comparison.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Logic.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Block.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.IfThenElse.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Extend.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Literal.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Multiple.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Restrict.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Select.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Update.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLength.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLit.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLoad.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorNew.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorSlice.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorStore.flix")
  }

  /**
    * Adds the test cases for the standard library.
    */
  private def addLibraryTests(flix: Flix): Unit = {
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestBigInt.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestFloat32.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestFloat64.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestInt8.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestInt16.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestInt32.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestInt64.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestList.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestMap.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestOption.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestPrelude.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestResult.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestSet.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestString.flix")
  }

  /**
    * Adds the abstract domains.
    */
  private def addAbstractDomains(flix: Flix): Unit = {
    flix.addPath("examples/domains/Belnap.flix")
    flix.addPath("examples/domains/Constant.flix")
    flix.addPath("examples/domains/ConstantParity.flix")
    flix.addPath("examples/domains/Interval.flix")
    flix.addPath("examples/domains/IntervalAlt.flix")
    flix.addPath("examples/domains/Mod3.flix")
    flix.addPath("examples/domains/Parity.flix")
    flix.addPath("examples/domains/ParitySign.flix")
    flix.addPath("examples/domains/PrefixSuffix.flix")
    flix.addPath("examples/domains/Sign.flix")
    flix.addPath("examples/domains/StrictSign.flix")
  }

}
