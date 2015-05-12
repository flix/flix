import java.io.File

import api.Flix
import org.scalatest.FunSuite

import scala.io.Source

class Examples extends FunSuite {
  def runExample(fileName: String) = {
    val flix = new Flix
    flix += Source.fromFile("src/examples/" + fileName + ".sexp").mkString
    flix.solve()
    flix.print()
  }

  val examples = new File("src/examples/").listFiles.map(_.getName).filter(_.endsWith(".sexp")).map(_.dropRight(5))

  for{name <- examples} test(name){ runExample(name) }
}
