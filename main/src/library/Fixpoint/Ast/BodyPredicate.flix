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
        case Guard1(v -> Bool, VarSym, SourceLocation)
        case Guard2((v, v) -> Bool, VarSym, VarSym, SourceLocation)
        case Guard3((v, v, v) -> Bool, VarSym, VarSym, VarSym, SourceLocation)
        case Guard4((v, v, v, v) -> v -> Bool, VarSym, VarSym, VarSym, VarSym, SourceLocation)
        case Guard5((v, v, v, v, v) -> Bool, VarSym, VarSym, VarSym, VarSym, VarSym, SourceLocation)
    }
}
