import {ClassSym} from "./ClassSym";
import {Def} from "./Def";
import {Enum} from "./Enum";
import {Modifier} from "./Modifier";
import {TypeParam} from "./TypeParam";
import {TypeAlias} from "./TypeAlias";

export interface Api {
    classes: ClassesByNS
    // TODO: instances
    // TODO: signs
    defs: DefsByNS
    enums: EnumsByNs
    typealiases: TypeAliasesByNs
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

interface Class {
    sym: ClassSym
    doc: [String]
    mod: [Modifier]
    tparam: TypeParam
    //  superClasses: List[Ast.TypeConstraint], signatures: List[TypedAst.Sig], laws: List[TypedAst.Def], loc: SourceLocation
}

//   case class Instance(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, tpe: Type, tconstrs: List[Ast.TypeConstraint], defs: List[TypedAst.Def], ns: Name.NName, loc: SourceLocation)

//   case class Sig(sym: Symbol.SigSym, spec: TypedAst.Spec, impl: Option[TypedAst.Impl])
//  case class Spec(doc: Ast.Doc, ann: List[TypedAst.Annotation], mod: Ast.Modifiers, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, loc: SourceLocation)



