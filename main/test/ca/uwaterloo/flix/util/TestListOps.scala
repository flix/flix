/*
 * Copyright 2026 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.util.collection.ListOps
import org.scalatest.funsuite.AnyFunSuite

class TestListOps extends AnyFunSuite {

  test("fullOuterJoin.Empty.01") {
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List.empty[Int], List.empty[Int])(_ == _)
    assert(pairs == Nil)
    assert(lone1 == Nil)
    assert(lone2 == Nil)
  }

  test("fullOuterJoin.Empty.02") {
    // Everything in the second list is unpaired.
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List.empty[Int], List(1, 2))(_ == _)
    assert(pairs == Nil)
    assert(lone1 == Nil)
    assert(lone2 == List(1, 2))
  }

  test("fullOuterJoin.Empty.03") {
    // Everything in the first list is unpaired.
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 2), List.empty[Int])(_ == _)
    assert(pairs == Nil)
    assert(lone1 == List(1, 2))
    assert(lone2 == Nil)
  }

  test("fullOuterJoin.SameOrder.01") {
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 2, 3), List(1, 2, 3))(_ == _)
    assert(pairs == List((1, 1), (2, 2), (3, 3)))
    assert(lone1 == Nil)
    assert(lone2 == Nil)
  }

  test("fullOuterJoin.DifferentOrder.01") {
    // The whole point: order must not affect which elements pair up.
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 2, 3), List(3, 1, 2))(_ == _)
    assert(pairs == List((1, 1), (2, 2), (3, 3)))
    assert(lone1 == Nil)
    assert(lone2 == Nil)
  }

  test("fullOuterJoin.PartialOverlap.01") {
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 2, 3), List(3, 4, 1))(_ == _)
    assert(pairs == List((1, 1), (3, 3)))
    assert(lone1 == List(2))
    assert(lone2 == List(4))
  }

  test("fullOuterJoin.Disjoint.01") {
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 2), List(3, 4))(_ == _)
    assert(pairs == Nil)
    assert(lone1 == List(1, 2))
    assert(lone2 == List(3, 4))
  }

  test("fullOuterJoin.Duplicates.01") {
    // Each element of the second list may be consumed at most once.
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 1), List(1))(_ == _)
    assert(pairs == List((1, 1)))
    assert(lone1 == List(1))
    assert(lone2 == Nil)
  }

  test("fullOuterJoin.Duplicates.02") {
    // Symmetric case: a surplus duplicate in the second list is left over.
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1), List(1, 1))(_ == _)
    assert(pairs == List((1, 1)))
    assert(lone1 == Nil)
    assert(lone2 == List(1))
  }

  test("fullOuterJoin.Duplicates.03") {
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 1, 2), List(2, 1, 1))(_ == _)
    assert(pairs == List((1, 1), (1, 1), (2, 2)))
    assert(lone1 == Nil)
    assert(lone2 == Nil)
  }

  test("fullOuterJoin.HeterogeneousTypes.01") {
    // The two lists need not have the same element type.
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 2, 3), List("3", "1", "4")) {
      case (i, s) => i.toString == s
    }
    assert(pairs == List((1, "1"), (3, "3")))
    assert(lone1 == List(2))
    assert(lone2 == List("4"))
  }

  test("fullOuterJoin.CoarseRelation.01") {
    // A relation coarser than equality: pair numbers by parity.
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 2), List(4, 3, 5)) {
      case (a, b) => a % 2 == b % 2
    }
    assert(pairs == List((1, 3), (2, 4)))
    assert(lone1 == Nil)
    assert(lone2 == List(5))
  }

  test("fullOuterJoin.NeverMatches.01") {
    val (pairs, lone1, lone2) = ListOps.fullOuterJoin(List(1, 2), List(1, 2))((_, _) => false)
    assert(pairs == Nil)
    assert(lone1 == List(1, 2))
    assert(lone2 == List(1, 2))
  }

}
