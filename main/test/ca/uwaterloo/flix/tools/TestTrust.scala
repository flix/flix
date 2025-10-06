package ca.uwaterloo.flix.tools

import org.scalatest.funsuite.AnyFunSuite
import ca.uwaterloo.flix.tools.pkg.Trust

class TestTrust extends AnyFunSuite {
  test("toString-ofString-java-interop") {
    val perm = Trust.JavaInterop
    val res = Trust.mkPermission(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-unsafe-cast") {
    val perm = Trust.UncheckedCast
    val res = Trust.mkPermission(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-effect") {
    val perm = Trust.Effect
    val res = Trust.mkPermission((perm.toString)) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }
}
