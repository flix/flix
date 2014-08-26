import java.io.File

import impl.ast.Compiler
import impl.ast.Parser

object Main {
  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    val ast = Parser.parse(file)
    println(ast)

    println(Compiler.compile(ast))
  }
}
