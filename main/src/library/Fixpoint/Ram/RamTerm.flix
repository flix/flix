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
    /// `Attribute(x, i)` represents an index into the tuple
    /// bound to the local variable `x`, i.e. `x[i]`.
    ///
    /// `Lat(x)` represents the lattice component in the tuple
    /// bound to the local variable `x`.
    ///
    pub enum RamTerm[v] {
        case Lit(v)
        case Attribute(RowVar, Int32)
        case Lat(RowVar)
        case Meet(v -> v -> v, RamTerm[v], RamTerm[v])
        case App0(Unit -> v)
        case App1(v -> v, RamTerm[v])
        case App2(v -> v -> v, RamTerm[v], RamTerm[v])
        case App3(v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v])
        case App4(v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
        case App5(v -> v -> v -> v -> v -> v, RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v], RamTerm[v])
    }

    instance ToString[RamTerm[v]] with ToString[v] {
        pub def toString(term: RamTerm[v]): String = match term {
            case Lit(v) => ToString.toString(v)
            case Attribute(var, index) => "${var}[${index}]"
            case Lat(var) => "${var}[-1]"
            case Meet(_, lhs, rhs) => "(${lhs} ⊓ ${rhs})"
            case App0(_) => "f()"
            case App1(_, v) => "f(${v})"
            case App2(_, v1, v2) => "f(${v1}, ${v2})"
            case App3(_, v1, v2, v3) => "f(${v1}, ${v2}, ${v3})"
            case App4(_, v1, v2, v3, v4) => "f(${v1}, ${v2}, ${v3}, ${v4})"
            case App5(_, v1, v2, v3, v4, v5) => "f(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }
}