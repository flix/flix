package ca.uwaterloo.flix.tools

import org.scalatest.funsuite.AnyFunSuite
import ca.uwaterloo.flix.tools.pkg.Permissions

class TestPermissions extends AnyFunSuite {
  test("toString-ofString-plain") {
    val perm = Permissions.PlainFlix
    val res = Permissions.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-trust-javaclass") {
    val perm = Permissions.TrustJavaClass
    val res = Permissions.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-unrestricted") {
    val perm = Permissions.Unrestricted
    val res = Permissions.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }
}
