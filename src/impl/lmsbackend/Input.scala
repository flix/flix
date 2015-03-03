package impl.lmsbackend

import Declarations._
import impl.logic

object Input {
  def convert(program: logic.Program): List[Rule] = {
//    assert(program.declarations.isEmpty, "NYI: lattice declarations on user-defined types")
    program.constraints.map{convert(_)}
  }

  def convert(constraint: logic.Constraint): Rule = {
    val ret = Rule(convert(constraint.head), constraint.body.map(convert(_)))
    collectLocalVars(ret).groupBy(_.name).values.filter(_.size > 1).foreach{vars =>
      error(s"local variable with inconsistent types: $vars")
    }
    ret
  }

  def convert(pred: logic.Predicate): GlobalLeqPred = {
    val tpe = convert(pred.typ)
    val global = GlobalVar(pred.name.s, tpe)
    val pattern = convert(pred.terms, tpe)
    GlobalLeqPred(pattern, global)
  }

  def convert(typ: logic.Type): Type = typ match {
    case logic.Type.Var(x) => ???
    case logic.Type.Unit => ???
    case logic.Type.Bool => BooleanType
    case logic.Type.Int => IntType
    case logic.Type.Str => StringType
    case logic.Type.Native => ???
    case logic.Type.Set(typ) => MapType(convert(typ), BooleanType)
    case logic.Type.Function(typ1, typ2) => MapType(convert(typ1), convert(typ2))
    case logic.Type.Tag(name, typ) => ???
    case logic.Type.Sum(ts) => ???
    case logic.Type.Tuple2(typ1, typ2) => TupleType(List(typ1, typ2).map(convert(_: logic.Type)))
    case logic.Type.Tuple3(typ1, typ2, typ3) => TupleType(List(typ1, typ2, typ3).map(convert(_: logic.Type)))
    case logic.Type.Tuple4(typ1, typ2, typ3, typ4) => TupleType(List(typ1, typ2, typ3, typ4).map(convert(_: logic.Type)))
    case logic.Type.Tuple5(typ1, typ2, typ3, typ4, typ5) => TupleType(List(typ1, typ2, typ3, typ4, typ5).map(convert(_: logic.Type)))
  }

  def typeCheck(pt: Type)(pattern: Pattern): Pattern = {
    if(pattern.tpe != pt) error(s"type mismatch: $pattern should have type $pt")
    pattern
  }

  def convert(terms: List[logic.Term], pt: Type): Pattern = pt match {
    case TupleType(elemTypes) => TuplePattern((terms zip elemTypes).map { case (term, pt) => convert(term, pt)})
    case MapType(keyType, valType) => terms.size match {
      case 1 => convert(terms(0), pt)
      case 2 => MapElem(convert(terms(0), keyType), convert(terms(1), valType))
      case _ => error(s"map type $pt should have 1 or 2 terms: $terms")
    }
    case _ => error(s"type mismatch: $terms should have type $pt")
  }

  def convert(term: logic.Term, pt: Type): Pattern = typeCheck(pt){ term match {
    case logic.Term.Unit => ???
    case logic.Term.Bool(b) => Constant(BooleanValue(b))
    case logic.Term.Int(i) => Constant(IntValue(i))
    case logic.Term.Str(s) => Constant(StringValue(s))
    case logic.Term.Set(xs) =>
      assert(xs.size == 1)
      val tpe = pt match {
        case MapType(tpe, BooleanType) => tpe
        case _ => error(s"type mismatch: set $term should have type $pt")
      }
      MapElem(convert(xs.head, tpe), Constant(BooleanValue(true)))
    case logic.Term.Var(name) => Variable(LocalVar(name.s, pt))
    case logic.Term.Abs(s, typ, t) => ???
    case logic.Term.App(t1, t2) => ???
    case logic.Term.IfThenElse(t1, t2, t3) => ???
    case logic.Term.Match(t, rules) => ???
    case logic.Term.UnaryOp(op, t1) => ???
    case logic.Term.BinaryOp(op, t1, t2) => ???
    case logic.Term.Tag(name, t, typ) => ???
    case logic.Term.Tuple2(t1, t2) => convert(List(t1, t2), pt)
    case logic.Term.Tuple3(t1, t2, t3) => convert(List(t1, t2, t3), pt)
    case logic.Term.Tuple4(t1, t2, t3, t4) => convert(List(t1, t2, t3, t4), pt)
    case logic.Term.Tuple5(t1, t2, t3, t4, t5) => convert(List(t1, t2, t3, t4, t5), pt)
  }}
}
