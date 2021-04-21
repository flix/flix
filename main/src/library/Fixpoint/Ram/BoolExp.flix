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
        case Guard6(v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard7(v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard8(v -> v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard9(v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard10(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard11(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard12(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard13(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard14(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case Guard15(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
    }

    instance ToString[BoolExp[v]] with ToString[v] {
        pub def toString(exp: BoolExp[v]): String =
            use Fixpoint.commaSeparate;
            match exp {
                case Empty(ramSym) => "${ramSym} = ∅"
                case NotMemberOf(terms, ramSym) => "(${commaSeparate(terms)}) ∉ ${ramSym}"
                case Eq(lhs, rhs) => "${lhs} = ${rhs}"
                case Leq(_, lhs, rhs) => "${lhs} ≤ ${rhs}"
                case Guard0(_) => "<clo>()"
                case Guard1(_, v) => "<clo>(${v})"
                case Guard2(_, v1, v2) => "<clo>(${v1}, ${v2})"
                case Guard3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
                case Guard4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case Guard5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
                case Guard6(_, v1, v2, v3, v4, v5, v6) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6})"
                case Guard7(_, v1, v2, v3, v4, v5, v6, v7) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7})"
                case Guard8(_, v1, v2, v3, v4, v5, v6, v7, v8) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8})"
                case Guard9(_, v1, v2, v3, v4, v5, v6, v7, v8, v9) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9})"
                case Guard10(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10})"
                case Guard11(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11})"
                case Guard12(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12})"
                case Guard13(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13})"
                case Guard14(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13}, ${v14})"
                case Guard15(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13}, ${v14}, ${v15})"
            }
    }
}