package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}

class TestStreaming extends AnyFunSuite with TestUtils {

  /**
    * The number of pieces each input is broken down into.
    */
  private val Chunks: Int = 100

  test("Stream01.simple-card-game") {
    val input = Files.readString(Paths.get("examples/simple-card-game.flix"))
    compile(input)
  }

  test("Stream02.the-ast-typing-problem-with-polymorphic-records") {
    val input = Files.readString(Paths.get("examples/the-ast-typing-problem-with-polymorphic-records.flix"))
    compile(input)
  }

  test("Stream03.using-channels-and-select") {
    val input = Files.readString(Paths.get("examples/using-channels-and-select.flix"))
    compile(input)
  }

  /**
    * We break the given string `input` down into a number of chunks and compile each of them.
    *
    * We simply test that compilation does not crash with an unexpected exception.
    */
  private def compile(input: String): Unit = {
    val length = input.length
    val step = length / Chunks

    val flix = new Flix()
    flix.compile()
    for (i <- 0 until Chunks) {
      val e = Math.min(i * step, length)
      val s = input.substring(0, e)
      flix.addSourceCode("<input>", s)
      flix.compile() // We simply care that this does not crash.
    }
  }

}
