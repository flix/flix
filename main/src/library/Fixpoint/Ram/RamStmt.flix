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

use Fixpoint.Ast.Constraint

mod Fixpoint.Ram {

    @Internal
    pub enum RamStmt[v] {
        case Insert(RelOp[v])
        case Merge(RamSym[v], RamSym[v])
        case Assign(RamSym[v], RamSym[v])
        case Purge(RamSym[v])
        case Seq(List[RamStmt[v]])
        case Until(List[BoolExp[v]], RamStmt[v])
        case Comment(String)
    }

    instance ToString[RamStmt[v]] {
        pub def toString(stmt: RamStmt[v]): String =
            let nl = String.lineSeparator();
            match stmt {
                case RamStmt.Insert(op) => ToString.toString(op)
                case RamStmt.Merge(src, dst) => "merge ${src} into ${dst}"
                case RamStmt.Assign(lhs, rhs) => "${lhs} := ${rhs}"
                case RamStmt.Purge(ramSym) => "purge ${ramSym}"
                case RamStmt.Seq(xs) => List.join(";${nl}", xs)
                case RamStmt.Until(test, body) =>
                    let tst = test |> List.join(" Λ ");
                    "until(${tst}) do${nl}${String.indent(4, "${body}")}end"
                case RamStmt.Comment(comment) => "// ${comment}"
            }
    }
}
