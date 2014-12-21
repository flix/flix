package impl.ast2

import java.io.File

import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}

object Runner {
  def main(args: Array[String]): Unit = {

    val line = Source.fromFile(new File(args(0))).getLines().mkString("\n")

    val parser = new Parser(line)
    parser.Root.run() match {
      case Success(ast) => {
        println(Compiler.compile(ast))
      }
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e) => println("Unexpected error during parsing run: " + e)
    }
  }
}
