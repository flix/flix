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
import {Kind} from "./Kind";

// TODO: Implement type.
export type Type = TypeVar | TypeCst | TypeApply

export interface TypeVar {
    tag: "Var"
    name: string
    kind: Kind
}

export interface TypeCst {
    tag: "Cst"
    tc: TypeConstructor
    kind: Kind
}

export interface TypeApply {
    tag: "Apply"
    tpe1: Type
    tpe2: Type
    kind: Kind
}

export type TypeConstructor = Bool | Int32

export interface Bool {
    tag: "Bool"
}

export interface Int32 {
    tag: "Int32"
}

