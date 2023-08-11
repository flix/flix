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
import {Class} from "./Class";
import {Def} from "./Def";
import {Enum} from "./Enum";
import {TypeAlias} from "./TypeAlias";

export interface Api {
    version: string
    namespaces: [string]
    classes: ClassesByNS
    enums: EnumsByNs
    typeAliases: TypeAliasesByNs
    defs: DefsByNS
}

type ClassesByNS = {
    [key: string]: [Class]
}

type EnumsByNs = {
    [key: string]: [Enum]
}

type TypeAliasesByNs = {
    [key: string]: [TypeAlias]
}

type DefsByNS = {
    [key: string]: [Def]
}
