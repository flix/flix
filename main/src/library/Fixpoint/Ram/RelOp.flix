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
    pub enum RelOp[v] {
        case Search(RowVar, RamSym[v], RelOp[v])
        case Query(RowVar, RamSym[v], Array[(Int32, RamTerm[v])], RelOp[v])
        case Project(Array[RamTerm[v]], RamSym[v])
        case If(List[BoolExp[v]], RelOp[v])
    }

    instance ToString[RelOp[v]] with ToString[v] {
        pub def toString(op: RelOp[v]): String =
            use Fixpoint.commaSeparate;
            let nl = String.lineSeparator();
            match op {
                case Search(var, ramSym, body) =>
                    "search ${var} ∈ ${ramSym} do${nl}${String.indent(4, "${body}")}end"
                case Query(var, ramSym, prefixQuery, body) =>
                    let query = Array.formatWith(match (i, term) -> {
                        ToString.toString(BoolExp.Eq(RamTerm.RowLoad(var, i), term))
                    }, " ∧ ", prefixQuery) as & Pure;
                    "query {${var} ∈ ${ramSym} | ${query}} do${nl}${String.indent(4, "${body}")}end"
                case Project(terms, ramSym) =>
                    "project (${commaSeparate(terms)}) into ${ramSym}"
                case If(test, then) =>
                    let tst =
                        List.map(ToString.toString, test) |>
                        String.intercalate(" ∧ ");
                    "if(${tst}) then${nl}${String.indent(4, "${then}")}end"
            }
    }
}