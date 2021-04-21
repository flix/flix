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
    pub enum BodyPredicate[v] {
        case BodyAtom(PredSym, Denotation[v], Polarity, Array[BodyTerm[v]], SourceLocation)
        case Guard0(Unit -> Bool, SourceLocation)
        case Guard1(v -> Bool, VarSym, SourceLocation)
        case Guard2(v -> v -> Bool, VarSym, VarSym, SourceLocation)
        case Guard3(v -> v -> v -> Bool, VarSym, VarSym, VarSym, SourceLocation)
        case Guard4(v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard5(v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard6(v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard7(v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard8(v -> v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard9(v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard10(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard11(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard12(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard13(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard14(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard15(v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> v -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
    }

    instance ToString[BodyPredicate[v]] with ToString[v] {
        pub def toString(body: BodyPredicate[v]): String =
            use Fixpoint.commaSeparate;
            match body {
                case BodyAtom(predSym, Relational, Positive, terms, _) => "${predSym}(${commaSeparate(terms)})"
                case BodyAtom(predSym, Relational, Negative, terms, _) => "not ${predSym}(${commaSeparate(terms)})"
                case BodyAtom(predSym, Latticenal(_), polarity, terms, _) =>
                    let len = Array.length(terms) as & Pure;
                    let k = terms[0..len-1] as & Pure;
                    let l = terms[len-1] as & Pure;
                    match polarity {
                        case Positive => "${predSym}(${commaSeparate(k)}; ${l})"
                        case Negative => "not ${predSym}(${commaSeparate(k)}; ${l})"
                    }
                case Guard0(_) => "<clo>()"
                case Guard1(_, v, _) => "<clo>(${v})"
                case Guard2(_, v1, v2, _) => "<clo>(${v1}, ${v2})"
                case Guard3(_, v1, v2, v3, _) => "<clo>(${v1}, ${v2}, ${v3})"
                case Guard4(_, v1, v2, v3, v4, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4})"
                case Guard5(_, v1, v2, v3, v4, v5, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5})"
                case Guard6(_, v1, v2, v3, v4, v5, v6, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6})"
                case Guard7(_, v1, v2, v3, v4, v5, v6, v7, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7})"
                case Guard8(_, v1, v2, v3, v4, v5, v6, v7, v8, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8})"
                case Guard9(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9})"
                case Guard10(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10})"
                case Guard11(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11})"
                case Guard12(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12})"
                case Guard13(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13})"
                case Guard14(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13}, ${v14})"
                case Guard15(_, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, _) => "<clo>(${v1}, ${v2}, ${v3}, ${v4}, ${v5}, ${v6}, ${v7}, ${v8}, ${v9}, ${v10}, ${v11}, ${v12}, ${v13}, ${v14}, ${v15})"
            }
    }
}