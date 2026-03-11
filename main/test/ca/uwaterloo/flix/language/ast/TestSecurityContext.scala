package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import org.scalatest.funsuite.AnyFunSuite

class TestSecurityContext extends AnyFunSuite {

  test("toString-ofString-paranoid") {
    val perm = SecurityContext.Paranoid
    val res = SecurityContext.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-plain") {
    val perm = SecurityContext.Plain
    val res = SecurityContext.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-unrestricted") {
    val perm = SecurityContext.Unrestricted
    val res = SecurityContext.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("lub.01") {
    assertResult(SecurityContext.Paranoid)(SecurityContext.Paranoid.lub(SecurityContext.Paranoid))
  }

  test("lub.02") {
    assertResult(SecurityContext.Plain)(SecurityContext.Paranoid.lub(SecurityContext.Plain))
  }

  test("lub.03") {
    assertResult(SecurityContext.Unrestricted)(SecurityContext.Paranoid.lub(SecurityContext.Unrestricted))
  }

  test("lub.04") {
    assertResult(SecurityContext.Plain)(SecurityContext.Plain.lub(SecurityContext.Paranoid))
  }

  test("lub.05") {
    assertResult(SecurityContext.Plain)(SecurityContext.Plain.lub(SecurityContext.Plain))
  }

  test("lub.06") {
    assertResult(SecurityContext.Unrestricted)(SecurityContext.Plain.lub(SecurityContext.Unrestricted))
  }

  test("lub.07") {
    assertResult(SecurityContext.Unrestricted)(SecurityContext.Unrestricted.lub(SecurityContext.Paranoid))
  }

  test("lub.08") {
    assertResult(SecurityContext.Unrestricted)(SecurityContext.Unrestricted.lub(SecurityContext.Plain))
  }

  test("lub.09") {
    assertResult(SecurityContext.Unrestricted)(SecurityContext.Unrestricted.lub(SecurityContext.Unrestricted))
  }

  test("glb.01") {
    assertResult(SecurityContext.Paranoid)(SecurityContext.Paranoid.glb(SecurityContext.Paranoid))
  }

  test("glb.02") {
    assertResult(SecurityContext.Paranoid)(SecurityContext.Paranoid.glb(SecurityContext.Plain))
  }

  test("glb.03") {
    assertResult(SecurityContext.Paranoid)(SecurityContext.Paranoid.glb(SecurityContext.Unrestricted))
  }

  test("glb.04") {
    assertResult(SecurityContext.Paranoid)(SecurityContext.Plain.glb(SecurityContext.Paranoid))
  }

  test("glb.05") {
    assertResult(SecurityContext.Plain)(SecurityContext.Plain.glb(SecurityContext.Plain))
  }

  test("glb.06") {
    assertResult(SecurityContext.Plain)(SecurityContext.Plain.glb(SecurityContext.Unrestricted))
  }

  test("glb.07") {
    assertResult(SecurityContext.Paranoid)(SecurityContext.Unrestricted.glb(SecurityContext.Paranoid))
  }

  test("glb.08") {
    assertResult(SecurityContext.Plain)(SecurityContext.Unrestricted.glb(SecurityContext.Plain))
  }

  test("glb.09") {
    assertResult(SecurityContext.Unrestricted)(SecurityContext.Unrestricted.lub(SecurityContext.Unrestricted))
  }

}
