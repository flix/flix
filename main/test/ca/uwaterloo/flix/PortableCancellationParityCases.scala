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

case class PortableCancellationParityCase(id: String, expectedOutput: String, source: String)

object PortableCancellationParityCases {

  val All: List[PortableCancellationParityCase] = List(
    PortableCancellationParityCase(
      id = "child-structured-exception-cancels-sibling",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        region rc {
          |            spawn {
          |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
          |                ()
          |            } @ rc;
          |            spawn {
          |                let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |                throw Exn.mk((20, 22));
          |                ()
          |            } @ rc;
          |            ()
          |        };
          |        0
          |    } catch {
          |        case exn: (Int32, Int32) =>
          |            let (a, b) = Exn.payloadAs(exn);
          |            a + b
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableCancellationParityCase(
      id = "parent-structured-exception-cancels-sibling",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        region rc {
          |            spawn {
          |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
          |                ()
          |            } @ rc;
          |            throw Exn.mk((19, 23));
          |            ()
          |        };
          |        0
          |    } catch {
          |        case exn: (Int32, Int32) =>
          |            let (a, b) = Exn.payloadAs(exn);
          |            a + b
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableCancellationParityCase(
      id = "child-structured-exception-discards-structured-result",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        let p = region rc {
          |            spawn {
          |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
          |                ()
          |            } @ rc;
          |            spawn {
          |                let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |                throw Exn.mk((20, 22));
          |                ()
          |            } @ rc;
          |            (1, 2)
          |        };
          |        let (a, b) = p;
          |        a + b
          |    } catch {
          |        case exn: (Int32, Int32) =>
          |            let (a, b) = Exn.payloadAs(exn);
          |            a + b
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableCancellationParityCase(
      id = "child-record-exception-cancels-sibling",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        region rc {
          |            spawn {
          |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
          |                ()
          |            } @ rc;
          |            spawn {
          |                let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |                throw Exn.mk({lhs = 20, rhs = 22});
          |                ()
          |            } @ rc;
          |            ()
          |        };
          |        0
          |    } catch {
          |        case exn: {lhs = Int32, rhs = Int32} =>
          |            let r = Exn.payloadAs(exn);
          |            r#lhs + r#rhs
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableCancellationParityCase(
      id = "parent-record-exception-cancels-sibling",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        region rc {
          |            spawn {
          |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
          |                ()
          |            } @ rc;
          |            throw Exn.mk({lhs = 19, rhs = 23});
          |            ()
          |        };
          |        0
          |    } catch {
          |        case exn: {lhs = Int32, rhs = Int32} =>
          |            let r = Exn.payloadAs(exn);
          |            r#lhs + r#rhs
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableCancellationParityCase(
      id = "child-result-record-exception-discards-record-result",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        let r: {lhs = Int32, rhs = Int32} = region rc {
          |            spawn {
          |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
          |                ()
          |            } @ rc;
          |            spawn {
          |                let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |                let r: Result[Int32, {lhs = Int32, rhs = Int32}] = Ok({lhs = 20, rhs = 22});
          |                throw Exn.mk(r);
          |                ()
          |            } @ rc;
          |            {lhs = 1, rhs = 2}
          |        };
          |        r#lhs + r#rhs
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
    PortableCancellationParityCase(
      id = "child-list-tuple-exception-cancels-sibling",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    try {
          |        region rc {
          |            spawn {
          |                Timer.runWithIO(() -> Timer.sleepMillis(5000i64));
          |                ()
          |            } @ rc;
          |            spawn {
          |                let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |                throw Exn.mk((10, 11) :: (9, 12) :: Nil);
          |                ()
          |            } @ rc;
          |            ()
          |        };
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
  )

}
