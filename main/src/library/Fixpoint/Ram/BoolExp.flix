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
    pub enum BoolExp[v] {
        case Empty(RamSym[v])
        case NotMemberOf(Array[RamTerm[v]], RamSym[v])
        case Eq(RamTerm[v], RamTerm[v])
        case Leq(v -> v -> Bool, RamTerm[v], RamTerm[v])
        case Guard0(Unit -> Bool)
        case Guard1(v -> Bool, RamTerm[v])
        case Guard2(v -> v -> Bool, RamTerm[v], RamTerm[v])
        case Guard3(v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard4(v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard5(v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
    }

    instance ToString[BoolExp[v]] with ToString[v] {
        pub def toString(exp: BoolExp[v]): String =
            use Fixpoint.commaSeparate;
            match exp {
                case Empty(ramSym) => "${ramSym} == ∅"
                case NotMemberOf(terms, ramSym) => "(${commaSeparate(terms)}) ∉ ${ramSym}"
                case Eq(lhs, rhs) => "${lhs} == ${rhs}"
                case Leq(_, lhs, rhs) => "${lhs} ≤ ${rhs}"
                case Guard0(_) => "<clo>()"
                case Guard1(_, v) => "<clo>(${v})"
                case Guard2(_, v1, v2) => "<clo>(${v1}, ${v2})"
                case Guard3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
                case Guard4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            }
    }
}