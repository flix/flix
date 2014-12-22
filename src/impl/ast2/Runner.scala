package impl.ast2

import java.nio.file.Paths

object Runner {

  def main(args: Array[String]): Unit = {
    val ast = Parser.parse(Paths.get(args(0)))
    println(Compiler.compile(ast))
  }

}
