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
        case BodyAtom(PredSym, Polarity, Array[BodyTerm[v]], SourceLocation)
        case GuardAtom(Array[v] -> Bool, Array[BodyTerm[v]], SourceLocation)
    }

    instance ToString[BodyPredicate[v]] with [v : ToString] {
        pub def toString(body: BodyPredicate[v]): String =
            use Fixpoint.commaSeparate;
            match body {
                case BodyAtom(predSym, Positive, terms, _) => "${predSym}(${commaSeparate(terms)})"
                case BodyAtom(predSym, Negative, terms, _) => "not ${predSym}(${commaSeparate(terms)})"
                case GuardAtom(_, terms, _) => "if f(${commaSeparate(terms)})"
            }
    }
}