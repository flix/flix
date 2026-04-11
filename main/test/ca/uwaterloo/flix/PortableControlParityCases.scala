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

case class PortableControlParityCase(id: String, expectedOutput: String, source: String)

object PortableControlParityCases {

  val All: List[PortableControlParityCase] = List(
    PortableControlParityCase(
      id = "custom-handler-suspend-then-return-tuple",
      expectedOutput = "42",
      source =
        """
          |eff Ask {
          |    def ask(): (Int32, Int32)
          |}
          |
          |def main(): Int32 \ IO =
          |    run {
          |        let p = Ask.ask();
          |        let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |        let (a, b) = p;
          |        a + b
          |    } with handler Ask {
          |        def ask(k) = k((20, 22))
          |    }
          |""".stripMargin,
    ),
    PortableControlParityCase(
      id = "default-handler-suspend-then-return-tuple",
      expectedOutput = "42",
      source =
        """
          |eff Ask {
          |    def ask(): (Int32, Int32)
          |}
          |
          |mod Ask {
          |    @DefaultHandler
          |    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Ask) + IO =
          |        run {
          |            f()
          |        } with handler Ask {
          |            def ask(k) = k((21, 21))
          |        }
          |}
          |
          |def main(): Int32 \ IO =
          |    Ask.runWithIO(() -> {
          |        let p = Ask.ask();
          |        let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |        let (a, b) = p;
          |        a + b
          |    })
          |""".stripMargin,
    ),
    PortableControlParityCase(
      id = "custom-handler-suspend-then-throw-tuple",
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
          |            throw Exn.mk((x, 2));
          |            0
          |        } with handler Ask {
          |            def ask(k) = k(40)
          |        }
          |    } catch {
          |        case exn: (Int32, Int32) =>
          |            let (a, b) = Exn.payloadAs(exn);
          |            a + b
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableControlParityCase(
      id = "default-handler-suspend-then-throw-tuple",
      expectedOutput = "42",
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
          |            def ask(k) = k(39)
          |        }
          |}
          |
          |def main(): Int32 \ IO =
          |    try {
          |        Ask.runWithIO(() -> {
          |            let x = Ask.ask();
          |            let _ = Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |            throw Exn.mk((x, 3));
          |            0
          |        })
          |    } catch {
          |        case exn: (Int32, Int32) =>
          |            let (a, b) = Exn.payloadAs(exn);
          |            a + b
          |        case _: Exn => -1
          |    }
          |""".stripMargin,
    ),
    PortableControlParityCase(
      id = "region-exit-suspend-then-return-tuple",
      expectedOutput = "42",
      source =
        """
          |def main(): Int32 \ IO =
          |    region rc {
          |        spawn {
          |            Timer.runWithIO(() -> Timer.sleepMillis(1i64));
          |            ()
          |        } @ rc;
          |        let p = (20, 22);
          |        let (a, b) = p;
          |        a + b
          |    }
          |""".stripMargin,
    ),
  )

}
