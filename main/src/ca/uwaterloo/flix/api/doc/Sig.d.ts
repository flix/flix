/*
 * Copyright 2021 Magnus Madsen
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
import {SigSym} from "./SigSym";
import {TypeParam} from "./TypeParam";
import {FormalParam} from "./FormalParam";
import {Modifier} from "./Modifier";
import {Type} from "./Type";
import {SourceLocation} from "./SourceLocation";
import {TypeConstraint} from "./TypeConstraint";

export interface Sig {
    sym: SigSym
    doc: [string]
    mod: [Modifier]
    tparams: [TypeParam]
    fparams: [FormalParam]
    tpe: Type
    eff: Type
    tcs: [TypeConstraint]
    loc: SourceLocation
    implemented: boolean
}
