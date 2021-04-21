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
        case App6(v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App7(v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App8(v -> v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)        
        case App9(v -> v -> v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App10(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App11(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App12(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App13(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App14(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case App15(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
    }

    instance ToString[HeadTerm[v]] with ToString[v] {
        pub def toString(term: HeadTerm[v]): String = match term {
            case HeadTerm.Var(varSym, _) => "${varSym}"
            case HeadTerm.Lit(v, _) => "${v}"
            case HeadTerm.App0(_) => "<clo>()"
            case HeadTerm.App1(_, v, _) => "<clo>(${v})"
            case HeadTerm.App2(_, v1, v2, _) => "<clo>(${v1}, ${v2})"
            case HeadTerm.App3(_, v1, v2, v3, _) => "<clo>(${v1}, ${v2}, ${v3})"
            case HeadTerm.App4(_, v1, v2, v3, v4, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
            case HeadTerm.App5(_, v1, v2, v3, v4, v5, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
            case HeadTerm.App6(_, v1, v2, v3, v4, v5, v6, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6})"
            case HeadTerm.App7(_, v1, v2, v3, v4, v5, v6, v7, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7})"
            case HeadTerm.App8(_, v1, v2, v3, v4, v5, v6, v7, v8, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8})"
            case HeadTerm.App9(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9})"
            case HeadTerm.App10(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10})"
            case HeadTerm.App11(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11})"
            case HeadTerm.App12(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12})"
            case HeadTerm.App13(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13})"
            case HeadTerm.App14(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13}, ${v14})"
            case HeadTerm.App15(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13}, ${v14}, ${v15})"
        }
    }
}