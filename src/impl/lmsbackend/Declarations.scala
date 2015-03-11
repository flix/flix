package impl.lmsbackend

class Error(msg: String) extends RuntimeException(msg)

object Declarations {
  def error(msg: String): Nothing = throw new Error("Error: " + msg)

  /** Types.
    *
    */
  sealed abstract class Type {
    def assertTypes(v1: Value, v2: Value): Unit = {
      assert(v1.tpe == this)
      assert(v2.tpe == this)
    }
    def leq(v1: Value, v2: Value): Boolean
    def meet(v1: Value, v2: Value): Value
    def join(v1: Value, v2: Value): Value
    def bot: Value
  }

  case object BooleanType extends Type {
    def leq(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      BooleanValue(false) == v1 || BooleanValue(true) == v2
    }
    def meet(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      BooleanValue(v1.asInstanceOf[BooleanValue].b && v2.asInstanceOf[BooleanValue].b)
    }
    def join(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      BooleanValue(v1.asInstanceOf[BooleanValue].b || v2.asInstanceOf[BooleanValue].b)
    }
    def bot = BooleanValue(false)
  }

  case object IntType extends Type {
    def leq(v1: Value, v2: Value) = error("leq not defined for integers")
    def meet(v1: Value, v2: Value) = error("meet not defined for integers")
    def join(v1: Value, v2: Value) = error("join not defined for integers")
    def bot = error("bot not defined for integers")
  }
  
  case object StringType extends Type {
    def leq(v1: Value, v2: Value) = error("leq not defined for strings")
    def meet(v1: Value, v2: Value) = error("meet not defined for strings")
    def join(v1: Value, v2: Value) = error("join not defined for strings")
    def bot = error("bot not defined for strings")
  }

  case class TupleType(elems: List[Type]) extends Type {
    def leq(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      (v1.asInstanceOf[TupleValue].elems zip v2.asInstanceOf[TupleValue].elems).forall{
        case (vv1, vv2) => vv1.leq(vv2)
      }
    }
    def meet(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      TupleValue((v1.asInstanceOf[TupleValue].elems zip v2.asInstanceOf[TupleValue].elems).map{
        case (vv1, vv2) => vv1.meet(vv2)
      })
    }
    def join(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      TupleValue((v1.asInstanceOf[TupleValue].elems zip v2.asInstanceOf[TupleValue].elems).map{
        case (vv1, vv2) => vv1.join(vv2)
      })
    }
    def bot = TupleValue(elems.map(_.bot))
  }
  case class MapType(keyType: Type, valueType: Type) extends Type {
    def leq(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      val vv1 = v1.asInstanceOf[MapValue].map
      val vv2 = v2.asInstanceOf[MapValue].map
      (vv1.keySet ++ vv2.keySet).forall{
        case key => vv1.getOrElse(key, valueType.bot).leq(vv2.getOrElse(key, valueType.bot))
      }
    }
    def meet(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      val vv1 = v1.asInstanceOf[MapValue].map
      val vv2 = v2.asInstanceOf[MapValue].map
      val newMap = (vv1.keySet & vv2.keySet).map{
        case key => (key, vv1(key).meet(vv2(key)))
      }.toMap
      MapValue(newMap, v1.asInstanceOf[MapValue].keyType, v1.asInstanceOf[MapValue].valType)
    }
    def join(v1: Value, v2: Value) = {
      assertTypes(v1, v2)
      val vv1 = v1.asInstanceOf[MapValue].map
      val vv2 = v2.asInstanceOf[MapValue].map
      val bot = v1.asInstanceOf[MapValue].valType.bot
      val newMap = (vv1.keySet ++ vv2.keySet).map{
        case key => (key, vv1.getOrElse(key, bot).join(vv2.getOrElse(key, bot)))
      }.toMap
      MapValue(newMap, v1.asInstanceOf[MapValue].keyType, v1.asInstanceOf[MapValue].valType)
    }
    def bot = MapValue(Map[Value, Value](), keyType, valueType)
  }
  def SetType(elem: Type) = MapType(elem, BooleanType)

  /** Values.
    *
    */
  sealed abstract class Value(val tpe: Type) {
    def leq(other: Value) = tpe.leq(this, other)
    def meet(other: Value) = tpe.meet(this, other)
    def join(other: Value) = tpe.join(this, other)
    def show: String
  }
  case class BooleanValue(val b: Boolean) extends Value(BooleanType) {
    def show = b.toString
  }
  case class IntValue(val i: Int) extends Value(IntType) {
    def show = i.toString
  }
  case class StringValue(val s: String) extends Value(StringType) {
    def show = '"' + s.toString + '"'
  }
  case class TupleValue(val elems: List[Value]) extends Value(TupleType(elems.map(_.tpe))) {
    def show = elems.map(_.show).mkString("<", ", ", ">")
  }
  case class MapValue(val map: Map[Value, Value], val keyType: Type, val valType: Type)
    extends Value(MapType(keyType, valType)) {
    def show = {
      def valString(key: Value) = if(valType == BooleanType) "" else " -> " + map(key).show
      map.keys.map{key => key.show + valString(key)}.mkString("{", ", ", "}")
    }
  }

  /** Vars.
    *
    */
  case class LocalVar(name: String, tpe: Type) {
    def show = s"$name: $tpe"
  }
  case class GlobalVar(name: String, tpe: Type) {
    def show = s"$name: $tpe"
  }

  /** Rule.
    *
    */
  case class Rule(head: GlobalLeqPred, body: List[Predicate])
  def collectLocalVars(rule: Rule): Set[LocalVar] = (rule.head :: rule.body).flatMap{collectLocalVars(_)}.toSet

  /** Predicates.
    *
    */
  sealed abstract class Predicate
  case class GlobalLeqPred(pattern: Pattern, variable: GlobalVar) extends Predicate
  case class LocalLeqPred(pattern: Pattern, variable: LocalVar) extends Predicate
  case class FnCallPred(result: LocalVar, function: Value=>Value, argument: Pattern) extends Predicate

  def collectLocalVars(pred: Predicate): Set[LocalVar] = pred match {
    case GlobalLeqPred(pattern, variable) => collectLocalVars(pattern)
    case LocalLeqPred(pattern, variable) => collectLocalVars(pattern) + variable
    case FnCallPred(result, function, argument) => collectLocalVars(argument) + result
  }

  /** Patterns.
    *
    */
  sealed abstract class Pattern(val tpe: Type) {
    def eval(valuation: PartialFunction[LocalVar, Value]): Option[Value]
  }
  case class Constant(v: Value) extends Pattern(v.tpe) {
    def eval(valuation: PartialFunction[LocalVar, Value]): Option[Value] = Some(v)
  }
  case class Variable(l: LocalVar) extends Pattern(l.tpe) {
    def eval(valuation: PartialFunction[LocalVar, Value]): Option[Value] = valuation.lift(l)
  }
  case class TuplePattern(ps: List[Pattern]) extends Pattern(TupleType(ps.map(_.tpe))) {
    def eval(valuation: PartialFunction[LocalVar, Value]): Option[Value] = {
      val pvals = ps.map(_.eval(valuation)).collect{case Some(v) => v}
      if(pvals.size == ps.size) Some(TupleValue(pvals)) else None
    }
  }
  case class MapElem(key: Pattern, value: Pattern) extends Pattern(MapType(key.tpe, value.tpe)) {
    def eval(valuation: PartialFunction[LocalVar, Value]): Option[Value] =
      for{
        keyVal <- key.eval(valuation)
        valueVal <- value.eval(valuation)
      } yield MapValue(Map(keyVal -> valueVal), key.tpe, value.tpe)
  }

  def collectLocalVars(pattern: Pattern): Set[LocalVar] = pattern match {
    case pattern: Constant => Set()
    case pattern: Variable => Set(pattern.l)
    case pattern: TuplePattern => pattern.ps.flatMap(collectLocalVars).toSet
    case pattern: MapElem => collectLocalVars(pattern.key) ++ collectLocalVars(pattern.value)
  }

}
