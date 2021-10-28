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

export interface Sig {
    sym: SigSym
    implemented: boolean
}
// TODO: This is not done
//   case class Sig(sym: Symbol.SigSym)
//  case class Spec(doc: Ast.Doc, ann: List[TypedAst.Annotation], mod: Ast.Modifiers, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, loc: SourceLocation)

