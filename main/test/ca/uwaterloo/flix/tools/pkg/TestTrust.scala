package ca.uwaterloo.flix.tools.pkg

import org.scalatest.funsuite.AnyFunSuite

class TestTrust extends AnyFunSuite {

  test("toString-ofString-paranoid") {
    val perm = Trust.Paranoid
    val res = Trust.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

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
    assertResult(Trust.Paranoid)(Trust.Paranoid.lub(Trust.Paranoid))
  }

  test("lub.02") {
    assertResult(Trust.Plain)(Trust.Paranoid.lub(Trust.Plain))
  }

  test("lub.03") {
    assertResult(Trust.Unrestricted)(Trust.Paranoid.lub(Trust.Unrestricted))
  }

  test("lub.04") {
    assertResult(Trust.Plain)(Trust.Plain.lub(Trust.Paranoid))
  }

  test("lub.05") {
    assertResult(Trust.Plain)(Trust.Plain.lub(Trust.Plain))
  }

  test("lub.06") {
    assertResult(Trust.Unrestricted)(Trust.Plain.lub(Trust.Unrestricted))
  }

  test("lub.07") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.lub(Trust.Paranoid))
  }

  test("lub.08") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.lub(Trust.Plain))
  }

  test("lub.09") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.lub(Trust.Unrestricted))
  }

  test("glb.01") {
    assertResult(Trust.Paranoid)(Trust.Paranoid.glb(Trust.Paranoid))
  }

  test("glb.02") {
    assertResult(Trust.Paranoid)(Trust.Paranoid.glb(Trust.Plain))
  }

  test("glb.03") {
    assertResult(Trust.Paranoid)(Trust.Paranoid.glb(Trust.Unrestricted))
  }

  test("glb.04") {
    assertResult(Trust.Paranoid)(Trust.Plain.glb(Trust.Paranoid))
  }

  test("glb.05") {
    assertResult(Trust.Plain)(Trust.Plain.glb(Trust.Plain))
  }

  test("glb.06") {
    assertResult(Trust.Plain)(Trust.Plain.glb(Trust.Unrestricted))
  }

  test("glb.07") {
    assertResult(Trust.Paranoid)(Trust.Unrestricted.glb(Trust.Paranoid))
  }

  test("glb.08") {
    assertResult(Trust.Plain)(Trust.Unrestricted.glb(Trust.Plain))
  }

  test("glb.09") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.lub(Trust.Unrestricted))
  }

}
