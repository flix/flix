package ca.uwaterloo.flix

import java.nio.file.Paths

object Main {

  def main(args: Array[String]): Unit = {

    Flix.solve(args map (arg => Paths.get(arg)))

  }

}
