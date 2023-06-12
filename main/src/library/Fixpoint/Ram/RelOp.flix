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

mod Fixpoint.Ram {

    @Internal
    pub enum RelOp[v] {
        case Search(RowVar, RamSym[v], RelOp[v])
        case Query(RowVar, RamSym[v], Vector[(Int32, RamTerm[v])], RelOp[v])
        case Functional(RowVar, Vector[v] -> Vector[Vector[v]], Vector[RamTerm[v]], RelOp[v])
        case Project(Vector[RamTerm[v]], RamSym[v])
        case If(Vector[BoolExp[v]], RelOp[v])
    }

    instance ToString[RelOp[v]] {
        pub def toString(op: RelOp[v]): String =
            let nl = String.lineSeparator();
            match op {
                case RelOp.Search(var, ramSym, body) =>
                    "search ${var} ∈ ${ramSym} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Query(var, ramSym, prefixQuery, body) =>
                    let query = Vector.joinWith(match (i, term) -> {
                        ToString.toString(BoolExp.Eq(RamTerm.RowLoad(var, i), term))
                    }, " ∧ ", prefixQuery);
                    "query {${var} ∈ ${ramSym} | ${query}} do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Functional(rowVar, _, terms, body) =>
                    "loop(${rowVar} <- f(${terms |> Vector.join(", ")})) do${nl}${String.indent(4, "${body}")}end"
                case RelOp.Project(terms, ramSym) =>
                    "project (${terms |> Vector.join(", ")}) into ${ramSym}"
                case RelOp.If(test, then) =>
                    let tst = test |> Vector.join(" ∧ ");
                    "if(${tst}) then${nl}${String.indent(4, "${then}")}end"
            }
    }
}
