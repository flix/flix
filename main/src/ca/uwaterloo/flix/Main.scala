package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.{Shell, Options}

object Main {

  def main(args: Array[String]): Unit = {

    implicit val options = Options()

    val shell = new Shell()
    shell.startAndAwait()

  }

}
