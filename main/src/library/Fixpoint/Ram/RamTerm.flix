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

    ///
    /// Represents a RAM term.
    ///
    /// `RowLoad(x, i)` represents an index into the tuple
    /// bound to the local variable `x`, i.e. `x[i]`.
    ///
    /// `LoadLatVar(x)` represents the lattice component in the tuple
    /// bound to the local variable `x`.
    ///
    @Internal
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
    }

    instance ToString[RamTerm[v]] {
        pub def toString(term: RamTerm[v]): String = match term {
            case RamTerm.Lit(v) => "%{v}"
            case RamTerm.RowLoad(var, index) => "${var}[${index}]"
            case RamTerm.LoadLatVar(var) => "${var}[-1]"
            case RamTerm.Meet(_, lhs, rhs) => "(${lhs} ⊓ ${rhs})"
            case RamTerm.App0(_) => "<clo>()"
            case RamTerm.App1(_, v) => "<clo>(${v})"
            case RamTerm.App2(_, v1, v2) => "<clo>(${v1}, ${v2})"
            case RamTerm.App3(_, v1, v2, v3) => "<clo>(${v1}, ${v2}, ${v3})"
            case RamTerm.App4(_, v1, v2, v3, v4) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case RamTerm.App5(_, v1, v2, v3, v4, v5) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }
}
