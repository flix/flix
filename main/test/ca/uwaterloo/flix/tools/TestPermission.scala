package ca.uwaterloo.flix.tools

import org.scalatest.funsuite.AnyFunSuite
import ca.uwaterloo.flix.tools.pkg.Permission

class TestPermission extends AnyFunSuite{
  test("toString-ofString-java-interop") {
    val perm = Permission.JavaInterop
    val res = Permission.ofString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-unsafe-cast") {
    val perm = Permission.UnsafeCast
    val res = Permission.ofString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-effect") {
    val perm = Permission.Effect
    val res = Permission.ofString((perm.toString)) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }
}
