package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import org.scalatest.funsuite.AnyFunSuite

class TestCompilationMessage extends AnyFunSuite with TestUtils {

  test("TestCompilationMessage") {
    val expected =
      s"""-- ${TestCompilationMessage.kind} [E9999] -------------------------------------------------- ${TestCompilationMessage.source.name}
         |
         |>> ${TestCompilationMessage.summary}
         |
         |${NoFormatter.src(TestCompilationMessage.loc, "The code is highlighted here")}
         |
         |""".stripMargin

    val actual = TestCompilationMessage.messageWithLoc(NoFormatter)(None)

    assert(actual.replace("\r\n", "\n") == expected.replace("\r\n", "\n"))
  }


  private case object TestCompilationMessage extends CompilationMessage {
    override def code: ErrorCode = ErrorCode.E9999

    override def kind: CompilationMessageKind = CompilationMessageKind.TestError

    override def loc: SourceLocation = SourceLocation.Unknown

    override def summary: String = "This is the summary."

    override def message(formatter: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import formatter.*
      s""">> $summary
         |
         |${src(loc, "The code is highlighted here")}
         |
         |""".stripMargin
    }
  }

}
