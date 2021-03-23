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
namespace Fixpoint/Ast {
    pub enum HeadTerm[v] {
        case Var(VarSym, SourceLocation)
        case Lit(v, SourceLocation)
        case App0(Unit -> v, SourceLocation)
        case App1(v -> v, VarSym, SourceLocation)
        case App2(v -> v -> v, VarSym, VarSym, SourceLocation)
        case App3(v -> v -> v -> v, VarSym, VarSym, VarSym, SourceLocation)
        case App4(v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App5(v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
    }

    instance ToString[HeadTerm[v]] with [v : ToString] {
        pub def toString(term: HeadTerm[v]): String = match term {
            case HeadTerm.Var(varSym, _) => "${varSym}"
            case HeadTerm.Lit(v, _) => "${v}"
            case HeadTerm.App0(_) => "f()"
            case HeadTerm.App1(_, v, _) => "f(${v})"
            case HeadTerm.App2(_, v1, v2, _) => "f(${v1}, ${v2})"
            case HeadTerm.App3(_, v1, v2, v3, _) => "f(${v1}, ${v2}, ${v3})"
            case HeadTerm.App4(_, v1, v2, v3, v4, _) => "f(${v1}, ${v2}, ${v3}, ${v4})"
            case HeadTerm.App5(_, v1, v2, v3, v4, v5, _) => "f(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
        }
    }
}