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
        case Search(RowVar, RelSym, List[BoolExp[v]], RelOp[v])
        case Query(RowVar, RelSym, Array[RamTerm[v]], List[BoolExp[v]], RelOp[v])
        case Project(Array[RamTerm[v]], RelSym)
    }

    instance ToString[RelOp[v]] with [v : ToString] {
        pub def toString(op: RelOp[v]): String =
            let nl = String.lineSeparator();
            match op {
                case Search(var, relSym, where, body) =>
                    if (List.isEmpty(where))
                        "search ${var} ∈ ${relSym} do${nl}${String.indent(4, "${body}")}end"
                    else
                        let cond = List.map(ToString.toString, where) |> String.intercalate(" Λ ");
                        "search ${var} ∈ ${relSym} where${nl}${String.indent(4, "${cond}")}do${nl}${String.indent(4, "${body}")}end"
                case Query(var, relSym, prefixQuery, where, body) =>
                    let query =
                        List.mapWithIndex(term -> i -> {
                            ToString.toString(BoolExp.Eq(RamTerm.Attribute(var, i), term))
                        }, Array.toList(prefixQuery) as & Pure) |>
                        String.intercalate(" ∧ ");
                    if (List.isEmpty(where))
                        "query {${var} ∈ ${relSym} | ${query}} do${nl}${String.indent(4, "${body}")}end"
                    else
                        let cond = List.map(ToString.toString, where) |> String.intercalate(" Λ ");
                        "query {${var} ∈ ${relSym} | ${query}} where${nl}${String.indent(4, "${cond}")}do${nl}${String.indent(4, "${body}")}end"
                case Project(terms, relSym) =>
                    let ts = Array.toList(terms) as & Pure;
                    let tuple = List.map(ToString.toString, ts) |> String.intercalate(", ");
                    "project (${tuple}) into ${relSym}"
            }
    }
}