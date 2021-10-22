import {Class} from "./Class";
import {Def} from "./Def";
import {Enum} from "./Enum";
import {TypeAlias} from "./TypeAlias";

export interface Api {
    classes: ClassesByNS
    defs: DefsByNS
    enums: EnumsByNs
    typeAliases: TypeAliasesByNs
}

type ClassesByNS = {
    [key: string]: [Class]
}

type DefsByNS = {
    [key: string]: [Def]
}

type EnumsByNs = {
    [key: string]: [Enum]
}

type TypeAliasesByNs = {
    [key: string]: [TypeAlias]
}
