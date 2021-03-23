/*
 * Copyright 2021 Benjamin Dahse
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

namespace Fixpoint/Ram {
    pub enum RamStmt[v] {
        case Insert(RelOp[v])
        case Merge(RelSym, RelSym)
        case Assign(RelSym, RelSym)
        case Purge(RelSym)
        case Seq(List[RamStmt[v]])
        case Until(List[BoolExp[v]], RamStmt[v])
        case Comment(String)
    }

    instance Monoid[RamStmt[v]] {
        pub def empty(): RamStmt[v] = RamStmt.Seq(Nil)

        pub def combine(x: RamStmt[v], y: RamStmt[v]): RamStmt[v] = match (x, y) {
            case (RamStmt.Seq(xs), RamStmt.Seq(ys)) => RamStmt.Seq(xs ::: ys)
            case (_, RamStmt.Seq(ys)) => RamStmt.Seq(x :: ys)
            case (RamStmt.Seq(xs), _) => RamStmt.Seq(xs ::: y :: Nil)
            case _ => RamStmt.Seq(x :: y :: Nil)
        }
    }

    instance ToString[RamStmt[v]] with [v : ToString] {
        pub def toString(stmt: RamStmt[v]): String =
            let nl = String.lineSeparator();
            match stmt {
                case Insert(op) => ToString.toString(op)
                case Merge(src, dst) => "merge ${src} into ${dst}"
                case Assign(lhs, rhs) => "${lhs} := ${rhs}"
                case Purge(relSym) => "purge ${relSym}"
                case Seq(xs) => List.map(ToString.toString, xs) |> String.intercalate(";${nl}")
                case Until(test, body) =>
                    let tst = List.map(ToString.toString, test) |> String.intercalate(" Λ ");
                    "until(${tst}) do${nl}${String.indent(4, "${body}")}end"
                case Comment(comment) => "// ${comment}"
            }
    }
}