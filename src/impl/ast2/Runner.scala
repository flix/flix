package impl.ast2

import java.io.File

import org.parboiled2.ParseError

import scala.io.Source
import scala.util.{Failure, Success}

object Runner {
  def main(args: Array[String]): Unit = {

    val line = Source.fromFile(new File("src/examples/Sign.flix")).getLines().mkString("\n")

    val parser = new Parser(line)
    parser.Root.run() match {
      case Success(ast) => {
        println("Result: " + ast)

        val c = new Compiler(ast)

      }
      case Failure(e: ParseError) => println("Expression is not valid: " + parser.formatError(e))
      case Failure(e) => println("Unexpected error during parsing run: " + e)
    }
  }
}
