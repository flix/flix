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
        case Search(RowVar, RelSym[v], RelOp[v])
        case Query(RowVar, RelSym[v], Array[RamTerm[v]], RelOp[v])
        case Project(Array[RamTerm[v]], RelSym[v])
        case If(List[BoolExp[v]], RelOp[v])
    }

    instance ToString[RelOp[v]] with ToString[v] {
        pub def toString(op: RelOp[v]): String =
            use Fixpoint.commaSeparate;
            let nl = String.lineSeparator();
            match op {
                case Search(var, relSym, body) =>
                    "search ${var} ∈ ${relSym} do${nl}${String.indent(4, "${body}")}end"
                case Query(var, relSym, prefixQuery, body) =>
                    let query =
                        List.mapWithIndex(term -> i -> {
                            ToString.toString(BoolExp.Eq(RamTerm.Attribute(var, i), term))
                        }, Array.toList(prefixQuery) as & Pure) |>
                        String.intercalate(" ∧ ");
                    "query {${var} ∈ ${relSym} | ${query}} do${nl}${String.indent(4, "${body}")}end"
                case Project(terms, relSym) =>
                    "project (${commaSeparate(terms)}) into ${relSym}"
                case If(test, then) =>
                    let tst =
                        List.map(ToString.toString, test) |>
                        String.intercalate(" ∧ ");
                    "if(${tst}) then${nl}${String.indent(4, "${then}")}end"
            }
    }
}