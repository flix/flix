package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.MessageKind
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation}
import org.scalatest.funsuite.AnyFunSuite

class TestFormatter extends AnyFunSuite {

  test("ExplicitLineCall.01") {
    def message(): String = {
      import Formatter._
      val message: MessageKind = MessageKind("Test Error")
      val source: Ast.Source = SourceLocation.Unknown.source
      val formatter: Formatter = NoFormatter
      val s: String =
        s""">> Bad thing happened
           |
           |""".stripMargin
      formatter.addErrorLine(message, source, s)
    }

    val expected =
      s"""-- Test Error -------------------------------------------------- <unknown>
         |>> Bad thing happened
         |
         |""".stripMargin

    assertResult(expected)(message())
  }

  test("ImplicitLineCall.01") {
    def message(): String = {
      import Formatter._
      implicit val message: MessageKind = MessageKind("Test Error")
      implicit val source: Ast.Source = SourceLocation.Unknown.source
      val formatter: Formatter = NoFormatter
      implicit val s: String =
        s""">> Bad thing happened
           |
           |""".stripMargin
      s
    }

    val expected =
      s"""-- Test Error -------------------------------------------------- <unknown>
         |>> Bad thing happened
         |
         |""".stripMargin

    assertResult(expected)(message())
  }
}
