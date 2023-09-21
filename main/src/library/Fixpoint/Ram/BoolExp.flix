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

    @internal
    pub enum BoolExp[v] {
        case Empty(RamSym[v])
        case NotMemberOf(Vector[RamTerm[v]], RamSym[v])
        case Eq(RamTerm[v], RamTerm[v])
        case Leq(v -> v -> Bool, RamTerm[v], RamTerm[v])
        case Guard0(Unit -> Bool)
        case Guard1(v -> Bool, RamTerm[v])
        case Guard2(v -> v -> Bool, RamTerm[v], RamTerm[v])
        case Guard3(v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard4(v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard5(v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
    }

    instance ToString[BoolExp[v]] {
        pub def toString(exp: BoolExp[v]): String =
            match exp {
                case BoolExp.Empty(ramSym) => "${ramSym} == ∅"
                case BoolExp.NotMemberOf(terms, ramSym) => "(${terms |> Vector.join(", ")}) ∉ ${ramSym}"
                case BoolExp.Eq(lhs, rhs) => "${lhs} == ${rhs}"
                case BoolExp.Leq(_, lhs, rhs) => "${lhs} ≤ ${rhs}"
                case BoolExp.Guard0(_) => "<clo>()"
                case BoolExp.Guard1(_, v) => "<clo>(${v})"
                case BoolExp.Guard2(_, v1, v2) => "<clo>(${v1}, ${v2})"
                case BoolExp.Guard3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
                case BoolExp.Guard4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case BoolExp.Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            }
    }
}
