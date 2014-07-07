package impl.runtime

import impl.logic.{Type, Value, Symbol}

object Functions {

  /**
   * Evaluates the given function symbol `s` with the given arguments `args`.
   */
  def evaluate(s: Symbol.FunctionSymbol, args: List[Value]): Value = s.s match {
    case "Int.+" => Value.Int(args.foldLeft(0) {
      case (acc, Value.Int(i)) => acc + i
      case (acc, v) => throw Error.TypeError(Type.Int, v)
    })
    case "Int.-" => ???
    case "Int.LessEq" => ???
    case "Int.Greater" => ???
    case _ => throw Error.UnknownFunctionSymbol(s)
  }

}
