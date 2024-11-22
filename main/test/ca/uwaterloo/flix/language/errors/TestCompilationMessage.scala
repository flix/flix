package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import org.scalatest.funsuite.AnyFunSuite

class TestCompilationMessage extends AnyFunSuite with TestUtils {


  test("TestCompilationMessage") {
    val expected =
      s"""-- ${TestCompilationMessage.kind} -------------------------------------------------- ${TestCompilationMessage.source.name}
         |
         |>> ${TestCompilationMessage.summary}
         |
         |${NoFormatter.code(TestCompilationMessage.loc, "The code is highlighted here")}
         |
         |""".stripMargin

    val actual = TestCompilationMessage.messageWithLoc(NoFormatter)

    assert(actual.replace("\r\n", "\n") == expected.replace("\r\n", "\n"))
  }


  private case object TestCompilationMessage extends CompilationMessage {

    override def kind: String = "Test Error"

    override def loc: SourceLocation = SourceLocation.Unknown

    override def summary: String = "This is the summary."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> $summary
         |
         |${code(loc, "The code is highlighted here")}
         |
         |""".stripMargin
    }
  }

}
