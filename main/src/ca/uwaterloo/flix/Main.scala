package ca.uwaterloo.flix

import java.nio.file.Paths

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.util.{Shell, Options}

object Main {

  def main(args: Array[String]): Unit = {

    implicit val options = Options()

    val ast = Compiler.compile(args.map(arg => Paths.get(arg)))

//    val shell = new Shell()
//    shell.startAndAwait()

  }

}
