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

    ///
    /// Represents a RAM term.
    ///
    /// `RowLoad(x, i)` represents an index into the tuple
    /// bound to the local variable `x`, i.e. `x[i]`.
    ///
    /// `LoadLatVar(x)` represents the lattice component in the tuple
    /// bound to the local variable `x`.
    ///
    pub enum RamTerm[v] {
        case Lit(v)
        case RowLoad(RowVar, Int32)
        case LoadLatVar(RowVar)
        case Meet(v -> v -> v, RamTerm[v], RamTerm[v])
        case App0(Unit -> v)
        case App1(v -> v, RamTerm[v])
        case App2(v -> v -> v, RamTerm[v], RamTerm[v])
        case App3(v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v])
        case App4(v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App5(v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App6(v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App7(v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App8(v -> v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App9(v -> v -> v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App10(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App11(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App12(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App13(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App14(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App15(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
    }

    instance ToString[RamTerm[v]] with ToString[v] {
        pub def toString(term: RamTerm[v]): String = match term {
            case Lit(v) => ToString.toString(v)
            case RowLoad(var, index) => "${var}[${index}]"
            case LoadLatVar(var) => "${var}[-1]"
            case Meet(_, lhs, rhs) => "(${lhs} ⊓ ${rhs})"
            case App0(_) => "<clo>()"
            case App1(_, v) => "<clo>(${v})"
            case App2(_, v1, v2) => "<clo>(${v1}, ${v2})"
            case App3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
            case App4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case App5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            case App6(_, v1, v2, v3, v4, v5, v6) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6})"
            case App7(_, v1, v2, v3, v4, v5, v6, v7) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7})"
            case App8(_, v1, v2, v3, v4, v5, v6, v7, v8) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8})"
            case App9(_, v1, v2, v3, v4, v5, v6, v7, v8, v9) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9})"
            case App10(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10})"
            case App11(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11})"
            case App12(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12})"
            case App13(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13})"
            case App14(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13}, ${v14})"
            case App15(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13}, ${v14}, ${v15})"
        }
    }
}