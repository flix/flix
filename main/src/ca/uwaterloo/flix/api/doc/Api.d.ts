import {ClassSym} from "./ClassSym";
import {Modifier} from "./Modifier";
import {Enum} from "./Enum";
import {TypeParam} from "./TypeParam";

interface Api {
    classes: ClassesByNS
    enums: EnumsByNs
}

type ClassesByNS = {
    [key: string]: [Class]
}

type EnumsByNs = {
    [key: string]: [Enum]
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

//   case class Def(sym: Symbol.DefnSym, spec: TypedAst.Spec, impl: TypedAst.Impl)
//   case class Spec(doc: Ast.Doc, ann: List[TypedAst.Annotation], mod: Ast.Modifiers, tparams: List[TypedAst.TypeParam], fparams: List[TypedAst.FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, loc: SourceLocation)


