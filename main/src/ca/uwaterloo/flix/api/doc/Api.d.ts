interface Api {
    classes: Classes
    enums: [Enum]
}

type Classes = {
    [key: string]: Class
}

interface Class {
    sym: ClassSym
    doc: [String]
    mod: [Modifier]
    tparam: TypeParam
    //  superClasses: List[Ast.TypeConstraint], signatures: List[TypedAst.Sig], laws: List[TypedAst.Def], loc: SourceLocation
}

interface Enum {

}

interface ClassSym {
    namespace: [String]
    name: String
}

interface TypeParam {
    name: String,
    kind: Kind
}

type Kind = "Bool" | "Star"

type Modifier = "public"