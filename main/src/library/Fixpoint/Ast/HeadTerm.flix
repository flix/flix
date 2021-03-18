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
        case App(Array[v] -> v, Array[HeadTerm[v]], SourceLocation)
    }

    instance ToString[HeadTerm[v]] with [v : ToString] {
        pub def toString(term: HeadTerm[v]): String =
            use Fixpoint.commaSeparate;
            match term {
                case HeadTerm.Var(varSym, _) => "${varSym}"
                case HeadTerm.Lit(v, _) => "${v}"
                case HeadTerm.App(_, terms, _) => "f(${commaSeparate(terms)})"
            }
    }
}