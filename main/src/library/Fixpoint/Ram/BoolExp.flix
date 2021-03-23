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
        case Empty(RelSym)
        case NotMemberOf(Array[RamTerm[v]], RelSym)
        case Eq(RamTerm[v], RamTerm[v])
        case Guard0(Unit -> Bool)
        case Guard1(v -> Bool, RamTerm[v])
        case Guard2(v -> v -> Bool, RamTerm[v], RamTerm[v])
        case Guard3(v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard4(v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard5(v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
    }

    instance ToString[BoolExp[v]] with [v : ToString] {
        pub def toString(exp: BoolExp[v]): String =
            use Fixpoint.commaSeparate;
            match exp {
                case Empty(relSym) => "${relSym} = ∅"
                case NotMemberOf(terms, relSym) => "(${commaSeparate(terms)}) ∉ ${relSym}"
                case Eq(lhs, rhs) => "${lhs} = ${rhs}"
                case Guard0(_) => "if f()"
                case Guard1(_, v) => "if f(${v})"
                case Guard2(_, v1, v2) => "if f(${v1}, ${v2})"
                case Guard3(_, v1, v2, v3) => "if f(${v1}, ${v2}, ${v3})"
                case Guard4(_, v1, v2, v3, v4) => "if f(${v1}, ${v2}, ${v3}, ${v4})"
                case Guard5(_, v1, v2, v3, v4, v5) => "if f(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            }
    }
}