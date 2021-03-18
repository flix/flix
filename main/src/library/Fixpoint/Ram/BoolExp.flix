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
        case Guard(Array[v] -> Bool, Array[RamTerm[v]])
        case Eq(RamTerm[v], RamTerm[v])
    }

    instance ToString[BoolExp[v]] with [v : ToString] {
        pub def toString(exp: BoolExp[v]): String =
            use Fixpoint.commaSeparate;
            match exp {
                case Empty(relSym) => "${relSym} = ∅"
                case NotMemberOf(terms, relSym) => "(${commaSeparate(terms)}) ∉ ${relSym}"
                case Guard(_, terms) => "f(${commaSeparate(terms)})"
                case Eq(lhs, rhs) => "${lhs} = ${rhs}"
            }
    }
}