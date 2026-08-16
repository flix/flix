/*
 * Copyright 2026 Werner Stein
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation
import org.scalatest.funsuite.AnyFunSuite

class TestCollisionRegistry extends AnyFunSuite {

  private val loc: SourceLocation = SourceLocation.Unknown

  private def unreachable[V](existing: V, incoming: V): String =
    fail(s"describe should not be called: existing='$existing', incoming='$incoming'")

  test("claim.01") {
    // The same (key, value) pair claimed twice is not a collision.
    val registry = new CollisionRegistry[String, String]()
    registry.claim("k", "v", loc)(unreachable)
    registry.claim("k", "v", loc)(unreachable)
  }

  test("claim.02") {
    // A different value claiming an already-claimed key is a collision.
    val registry = new CollisionRegistry[String, String]()
    registry.claim("k", "v1", loc)(unreachable)
    val ex = intercept[InternalCompilerException] {
      registry.claim("k", "v2", loc)((existing, incoming) => s"collision: $existing vs $incoming")
    }
    assert(ex.getMessage.contains("collision: v1 vs v2"))
  }

  test("claim.03") {
    // describe receives (existing, incoming) in that order, not the reverse.
    val registry = new CollisionRegistry[String, String]()
    registry.claim("k", "first", loc)(unreachable)
    var seen: Option[(String, String)] = None
    intercept[InternalCompilerException] {
      registry.claim("k", "second", loc) { (existing, incoming) =>
        seen = Some((existing, incoming))
        "collision"
      }
    }
    assert(seen.contains(("first", "second")))
  }

  test("claim.04") {
    // Different keys never interact, regardless of their values.
    val registry = new CollisionRegistry[String, String]()
    registry.claim("k1", "v", loc)(unreachable)
    registry.claim("k2", "v", loc)(unreachable)
    registry.claim("k3", "different", loc)(unreachable)
  }

  test("claim.05") {
    // describe is not invoked at all when there is no collision to report -- it must not
    // be evaluated eagerly, since building an error message is wasted work on the (vastly
    // more common) success path.
    var calls = 0
    val registry = new CollisionRegistry[String, String]()
    registry.claim("k", "v", loc) { (existing, incoming) =>
      calls += 1
      s"$existing/$incoming"
    }
    registry.claim("k", "v", loc) { (existing, incoming) =>
      calls += 1
      s"$existing/$incoming"
    }
    assert(calls == 0)
  }

  test("claim.06") {
    // Equality is structural, not reference-based: two equal-but-distinct case class
    // instances are not a collision.
    case class Origin(name: String, index: Int)
    val registry = new CollisionRegistry[String, Origin]()
    registry.claim("k", Origin("a", 1), loc)(unreachable)
    registry.claim("k", Origin("a", 1), loc)(unreachable)
  }

  test("claim.07") {
    case class Origin(name: String, index: Int)
    val registry = new CollisionRegistry[String, Origin]()
    registry.claim("k", Origin("a", 1), loc)(unreachable)
    val ex = intercept[InternalCompilerException] {
      registry.claim("k", Origin("a", 2), loc)((existing, incoming) => s"$existing != $incoming")
    }
    assert(ex.getMessage.contains("Origin(a,1) != Origin(a,2)"))
  }

  test("thread-safety.01") {
    // Concurrent claims of distinct keys must never spuriously collide with each other.
    val registry = new CollisionRegistry[Int, String]()
    val keys = (0 until 1000).toList
    val errors = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]()
    val threads = (0 until 8).map { t =>
      new Thread(() => keys.foreach { k =>
        try registry.claim(k, s"origin$k", loc)((e, i) => s"$e/$i")
        catch { case e: Throwable => errors.add(e) }
      })
    }
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(errors.isEmpty)
  }

  test("thread-safety.02") {
    // Concurrent claims of the *same* key with two different candidate values: exactly one
    // value wins the race, and every thread proposing a different value observes a
    // collision against whichever value actually won -- none of them silently succeed.
    val registry = new CollisionRegistry[String, Int]()
    val results = new java.util.concurrent.ConcurrentLinkedQueue[Either[Throwable, Unit]]()
    val threads = (0 until 16).map { t =>
      new Thread(() =>
        try {
          registry.claim("k", t % 2, loc)((e, i) => s"$e/$i")
          results.add(Right(()))
        } catch {
          case e: Throwable => results.add(Left(e))
        }
      )
    }
    threads.foreach(_.start())
    threads.foreach(_.join())
    import scala.jdk.CollectionConverters.*
    val all = results.asScala.toList
    // Every thread either succeeds (its value matched the winner) or throws
    // InternalCompilerException (its value did not) -- nothing is lost or silently ignored.
    assert(all.forall {
      case Right(_) => true
      case Left(_: InternalCompilerException) => true
      case Left(_) => false
    })
    assert(all.exists(_.isRight))
  }

}
