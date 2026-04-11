/*
 * Copyright 2026 Magnus Madsen
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

package ca.uwaterloo.flix

case class PortableExceptionParityCase(id: String, expectedOutput: String, source: String)

object PortableExceptionParityCases {

  val All: List[PortableExceptionParityCase] = List(
    PortableExceptionParityCase(
      id = "typed-catch",
      expectedOutput = "21",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        throw Exn.mk(21);
          |        0
          |    } catch {
          |        case exn: Int32 => Exn.payloadAs(exn)
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "catch-order",
      expectedOutput = "1",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        throw Exn.mk(9);
          |        0
          |    } catch {
          |        case _: Exn => 1
          |        case _: Int32 => 2
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "no-match-propagates",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        let _ = try {
          |            throw Exn.mk(42);
          |            ()
          |        } catch {
          |            case _: String => ()
          |        };
          |        0
          |    } catch {
          |        case exn: Int32 => Exn.payloadAs(exn)
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "catch-binder-flow",
      expectedOutput = "33",
      source =
        """
          |def payloadOf(exn: Exn): Int32 = Exn.payloadAs(exn)
          |
          |def main(): Int32 \ IO =
          |    try {
          |        throw Exn.mk(33);
          |        0
          |    } catch {
          |        case exn: Int32 =>
          |            let saved = exn;
          |            payloadOf(saved)
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "catch-after-suspension",
      expectedOutput = "12",
      source =
        """
          |def payloadOf(exn: Exn): Int32 = Exn.payloadAs(exn)
          |
          |def main(): Int32 \ IO =
          |    try {
          |        throw Exn.mk(12);
          |        0
          |    } catch {
          |        case exn: Int32 =>
          |            let saved = exn;
          |            let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |            payloadOf(saved)
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "typed-catch-list-tuple",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        throw Exn.mk((10, 11) :: (9, 12) :: Nil);
          |        0
          |    } catch {
          |        case exn: List[(Int32, Int32)] =>
          |            match Exn.payloadAs(exn) {
          |                case (a, b) :: (c, d) :: Nil => a + b + c + d
          |                case _ => -2
          |            }
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "typed-catch-result-record",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        let r: Result[Int32, {lhs = Int32, rhs = Int32}] = Ok({lhs = 20, rhs = 22});
          |        throw Exn.mk(r);
          |        0
          |    } catch {
          |        case exn: Result[Int32, {lhs = Int32, rhs = Int32}] =>
          |            match Exn.payloadAs(exn) {
          |                case Ok(r) => r#lhs + r#rhs
          |                case Err(n) => n
          |            }
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "custom-handler-throw-after-resume",
      expectedOutput = "21",
      source =
        """
          |eff Ask {
          |    def ask(): Int32
          |}
          |
          |def main(): Int32 \ IO =
          |    try {
          |        run {
          |            let x = Ask.ask();
          |            throw Exn.mk(x + 1);
          |            0
          |        } with handler Ask {
          |            def ask(k) = k(20)
          |        }
          |    } catch {
          |        case exn: Int32 => Exn.payloadAs(exn)
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "custom-handler-suspend-then-throw",
      expectedOutput = "42",
      source =
        """
          |eff Ask {
          |    def ask(): Int32
          |}
          |
          |def main(): Int32 \ IO =
          |    try {
          |        run {
          |            let x = Ask.ask();
          |            let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |            throw Exn.mk(x + 2);
          |            0
          |        } with handler Ask {
          |            def ask(k) = k(40)
          |        }
          |    } catch {
          |        case exn: Int32 => Exn.payloadAs(exn)
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "default-handler-throw-after-resume",
      expectedOutput = "15",
      source =
        """
          |eff Ask {
          |    def ask(): Int32
          |}
          |
          |mod Ask {
          |    @DefaultHandler
          |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Ask) + IO =
          |        run {
          |            f()
          |        } with handler Ask {
          |            def ask(k) = k(5)
          |        }
          |}
          |
          |def main(): Int32 \ IO =
          |    try {
          |        Ask.runWithIO(() -> {
          |            let x = Ask.ask();
          |            throw Exn.mk(x + 10);
          |            0
          |        })
          |    } catch {
          |        case exn: Int32 => Exn.payloadAs(exn)
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableExceptionParityCase(
      id = "default-handler-suspend-then-throw",
      expectedOutput = "37",
      source =
        """
          |eff Ask {
          |    def ask(): Int32
          |}
          |
          |mod Ask {
          |    @DefaultHandler
          |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Ask) + IO =
          |        run {
          |            f()
          |        } with handler Ask {
          |            def ask(k) = k(7)
          |        }
          |}
          |
          |def main(): Int32 \ IO =
          |    try {
          |        Ask.runWithIO(() -> {
          |            let x = Ask.ask();
          |            let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |            throw Exn.mk(x + 30);
          |            0
          |        })
          |    } catch {
          |        case exn: Int32 => Exn.payloadAs(exn)
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
  )

}
