import {ClassSym} from "./ClassSym";
import {Modifier} from "./Modifier";

interface Api {
    classes: ClassesByNS
    enums: [Enum]
}

type ClassesByNS = {
    [key: string]: Class // TODO: What should key be?
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

//   case class Def(sym: Symbol.DefnSym, spec: TypedAst.Spec, impl: TypedAst.Impl)

//   case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypedAst.TypeParam], cases: Map[Name.Tag, TypedAst.Case], tpeDeprecated: Type, sc: Scheme, loc: SourceLocation)

//   case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypedAst.TypeParam], tpe: Type, loc: SourceLocation)

interface Enum {

}

interface TypeParam {
    name: String,
    kind: Kind
}

type Kind = "Bool" | "Star"

