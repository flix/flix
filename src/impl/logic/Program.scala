package impl.logic

/**
 * A program consists of a list of declarations and constraints.
 */
case class Program(declarations: List[Declaration], constraints: List[Constraint]) {
  /**
   * Returns the list of facts.
   */
  def facts: List[Constraint.Fact] = constraints collect {
    case f: Constraint.Fact => f
  }

  /**
   * Returns the list of rules.
   */
  def rules: List[Constraint.Rule] = constraints collect {
    case r: Constraint.Rule => r
  }

  /**
   * Optionally returns the bottom value associated with the given `baseType`.
   */
  def lookupBot(baseType: Type): Option[Value] = declarations.collectFirst {
    case Declaration.DeclareBot(bot, actualType) if actualType == baseType => bot
  }

  /**
   * Optionally returns the less-than-equal function associated with the given `baseType`.
   */
  def lookupLeq(baseType: Type): Option[Term.Abs] = {
    val targetType = Type.Function(baseType, Type.Function(baseType, Type.Bool))

    declarations.collectFirst {
      case Declaration.DeclareLeq(abs, actualType) if actualType == targetType => abs
    }
  }

  /**
   * Optionally returns the least-upper-bound function associated with the given `baseType`.
   */
  def lookupLub(baseType: Type): Option[Term.Abs] = {
    val targetType = Type.Function(baseType, Type.Function(baseType, baseType))

    declarations.collectFirst {
      case Declaration.DeclareLub(abs, actualType) if actualType == targetType => abs
    }
  }

  /**
   * Optionally returns the height function associated with the given `baseType`.
   */
  def lookupHeight(baseType: Type): Option[Term.Abs] = {
    val targetType = Type.Function(baseType, Type.Int)

    declarations.collectFirst {
      case Declaration.DeclareHeight(abs, actualType) if actualType == targetType => abs
    }
  }
}
