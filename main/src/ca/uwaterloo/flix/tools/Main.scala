package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{Options, Result}

// temporary file
object Main {
  def main(argv: Array[String]): Unit = {
    val simple =
      s"""
         |def main(): Int32 = 32
         |
         |@Test
         |def testMain1(): Bool = Assert.eq(main(), 32)
         |
       """.stripMargin
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addSourceCode("main", simple)
    flix.options = Options.Default.copy(progress = true)

    val ast = flix.check().unsafeGet
    MutationTester.run(ast) match {
      case Result.Ok((all, killed, compilationFailed)) => println("all = " + all + " killed = " + killed + " compilationFailed = " + compilationFailed)
      case Result.Err(_) => println("something went wrong")
    }
  }
}
