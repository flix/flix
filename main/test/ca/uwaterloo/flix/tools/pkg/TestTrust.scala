package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Formatter
import org.scalatest.funsuite.AnyFunSuite

import java.io.PrintStream
import java.nio.file.Files

class TestTrust extends AnyFunSuite {

  test("toString-ofString-plain") {
    val perm = Trust.Plain
    val res = Trust.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-unrestricted") {
    val perm = Trust.Unrestricted
    val res = Trust.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("lub.01") {
    assertResult(Trust.Plain)(Trust.Plain.lub(Trust.Plain))
  }

  test("lub.02") {
    assertResult(Trust.Unrestricted)(Trust.Plain.lub(Trust.Unrestricted))
  }

  test("lub.03") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.lub(Trust.Plain))
  }

  test("lub.04") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.lub(Trust.Unrestricted))
  }

  test("glb.01") {
    assertResult(Trust.Plain)(Trust.Plain.glb(Trust.Plain))
  }

  test("glb.02") {
    assertResult(Trust.Plain)(Trust.Plain.glb(Trust.Unrestricted))
  }

  test("glb.03") {
    assertResult(Trust.Plain)(Trust.Unrestricted.glb(Trust.Plain))
  }

  test("glb.04") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.glb(Trust.Unrestricted))
  }
}
