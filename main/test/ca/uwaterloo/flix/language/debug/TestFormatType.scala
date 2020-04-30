package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Type}
import org.scalatest.FunSuite

class TestFormatType extends FunSuite with TestUtils {
  test("FormatType.WellFormedType.External.01") {
    val tpe = Type.mkRecordExtend("x", Type.Int32, Type.mkRecordExtend("y", Type.Str, Type.RecordEmpty))

    val expected = "{ x: Int32, y: Str }"
    val actual = FormatType2.format(tpe)(FormatType2.Readability.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.External.02") {
    val rest = Type.Var(0, Kind.Star, Rigidity.Rigid)
    rest.setText("theRest")
    val tpe = Type.mkRecordExtend("x", Type.Int32, rest)

    val expected = "{ x: Int32 | theRest }"
    val actual = FormatType2.format(tpe)(FormatType2.Readability.External)

    assert(actual == expected)
  }
}
