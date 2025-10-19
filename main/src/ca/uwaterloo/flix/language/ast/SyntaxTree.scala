/*
 * Copyright 2024 Herluf Baggesen
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
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.Source

/**
  * Represents the source code of a compilation unit.
  *
  * A [[SyntaxTree]] is unstructured: it allows much more flexibility than later
  * abstract syntax trees. This flexibility is used to capture source code that may
  * contain faulty syntax. The tree has nodes that hold a [[TreeKind]] and zero or
  * more children. Each child is either a [[Child.TokenChild]] or a [[Child.TreeChild]].
  *
  * Note that [[SyntaxTree]] offers few guarantees. In particular:
  *   - There is no guarantee that a specific node is present or absent as a child.
  *   - There is no guarantee that a specific node has a specific number of children.
  */
object SyntaxTree {

  /**
    * A root containing syntax trees for multiple sources.
    */
  case class Root(units: Map[Source, Tree], tokens: Map[Source, Array[Token]])

  /**
    * The empty SyntaxTree
    */
  val empty: Root = Root(Map.empty, Map.empty)

  /**
    * A marker trait for a child node in a syntax tree.
    * In practice this is implemented by [[Tree]] and [[Token]].
    */
  trait Child

  /**
    * A node in a [[SyntaxTree]]
    *
    * @param kind     The kind of the node.
    * @param loc      The location that the node spans in the source file.
    * @param children The children of the node.
    */
  case class Tree(kind: TreeKind, var children: Array[Child], var loc: SourceLocation) extends Child


  /**
    * A common super-type for [[TreeKind]]s
    */
  sealed trait TreeKind

  /**
    * Different kinds of syntax nodes in a [[SyntaxTree]].
    *
    * The only error kind that holds data is the special [[TreeKind.ErrorTree]].
    */
  object TreeKind {
    /**
      * A special error kind wrapping a [[CompilationMessage]].
      */
    case class ErrorTree(error: CompilationMessage) extends TreeKind

    /**
      * A special [[TreeKind]] used as a placeholder when a new mark is opened and the actual kind is not yet known.
      * [[UnclosedMark]] always gets overwritten with another [[TreeKind]] as parsing happens.
      * Failure to do so is a compiler error and gets caught when building the syntax tree.
      */
    case object UnclosedMark extends TreeKind

    case object AnnotationList extends TreeKind

    case object Argument extends TreeKind

    case object ArgumentNamed extends TreeKind

    case object ArgumentList extends TreeKind

    case object Case extends TreeKind

    case object CommentList extends TreeKind

    case object DerivationList extends Type

    case object Doc extends TreeKind

    case object Ident extends TreeKind

    case object Kind extends TreeKind

    case object ModifierList extends TreeKind

    case object Operator extends TreeKind

    case object Parameter extends TreeKind

    case object ParameterList extends TreeKind

    case object PredicateAndArity extends TreeKind

    case object QName extends TreeKind

    case object Root extends TreeKind

    case object StructField extends TreeKind

    case object TrailingDot extends TreeKind

    case object TypeParameter extends TreeKind

    case object TypeParameterList extends TreeKind

    //////////////////////////////////////////////////////////////////////////////////////////
    // DECLARATIONS                                                                         //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Decl extends TreeKind

    object Decl {
      case object AssociatedTypeDef extends Decl

      case object AssociatedTypeSig extends Decl

      case object Trait extends Decl

      case object Def extends Decl

      case object Redef extends Decl

      case object Effect extends Decl

      case object Enum extends Decl

      case object EqualityConstraintList extends Decl

      case object EqualityConstraintFragment extends Decl

      case object Instance extends Decl

      case object Law extends Decl

      case object Module extends Decl

      case object Op extends Decl

      case object RestrictableEnum extends Decl

      case object Signature extends Decl

      case object Struct extends Decl

      case object TypeAlias extends Decl
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// EXPRESSIONS                                                                         //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Exp extends TreeKind

    object Exp {

      /**
        * A marker kind used to wrap nested expressions.
        */
      // For instance on a binary expression "1 + 2" you would do
      // Exp
      //   Binary
      //     Exp
      //       LiteralNumber
      //   Operator
      //     Exp
      //       LiteralNumber
      case object Exp extends Exp

      case object Apply extends Exp

      case object Ascribe extends Exp

      case object Binary extends Exp

      case object Block extends Exp

      case object CheckedEffectCast extends Exp

      case object CheckedTypeCast extends Exp

      case object DebugInterpolator extends Exp

      case object ExtMatch extends Exp

      case object ExtMatchRuleFragment extends Exp

      case object ExtTag extends Exp

      case object Index extends Exp

      case object IndexMut extends Exp

      case object InvokeConstructor extends Exp

      case object InvokeMethod extends Exp

      case object FixpointConstraint extends Exp

      case object FixpointConstraintSet extends Exp

      case object FixpointLambda extends Exp

      case object FixpointFromFragment extends Exp

      case object FixpointInject extends Exp

      case object FixpointQuery extends Exp

      case object FixpointQueryWithProvenance extends Exp

      case object FixpointSelect extends Exp

      case object FixpointSolveWithProject extends Exp

      case object FixpointSolveWithProvenance extends Exp

      case object FixpointWhere extends Exp

      case object FixpointWith extends Exp

      case object ForApplicative extends Exp

      case object Foreach extends Exp

      case object ForMonadic extends Exp

      case object ForFragmentGenerator extends Exp

      case object ForFragmentGuard extends Exp

      case object ForFragmentLet extends Exp

      case object GetField extends Exp

      case object Handler extends Exp

      case object Hole extends Exp

      case object HoleVariable extends Exp

      case object IfThenElse extends Exp

      case object InstanceOf extends Exp

      case object Intrinsic extends Exp

      case object JvmMethod extends Exp

      case object Lambda extends Exp

      case object LambdaExtMatch extends Exp

      case object LambdaMatch extends Exp

      case object LetMatch extends Exp

      case object Literal extends Exp

      case object LiteralArray extends Exp

      case object LiteralList extends Exp

      case object LiteralMap extends Exp

      case object LiteralMapKeyValueFragment extends Exp

      case object LiteralStructFieldFragment extends Exp

      case object LiteralSet extends Exp

      case object LiteralVector extends Exp

      case object LocalDef extends Exp

      case object Match extends Exp

      case object MatchRuleFragment extends Exp

      case object NewObject extends Exp

      case object NewStruct extends Exp

      case object StructGet extends Exp

      case object StructPut extends Exp

      case object StructPutRHS extends Exp

      case object OpenVariant extends Exp

      case object OpenVariantAs extends Exp

      case object Paren extends Exp

      case object ParYield extends Exp

      case object ParYieldFragment extends Exp

      case object RecordOperation extends Exp

      case object RecordOpExtend extends Exp

      case object RecordOpRestrict extends Exp

      case object RecordOpUpdate extends Exp

      case object RecordSelect extends Exp

      case object RestrictableChoose extends Exp

      case object RestrictableChooseStar extends Exp

      case object Run extends Exp

      case object Region extends Exp

      case object RegionName extends Exp

      case object Select extends Exp

      case object SelectRuleFragment extends Exp

      case object SelectRuleDefaultFragment extends Exp

      case object Spawn extends Exp

      case object Statement extends Exp

      case object Static extends Exp

      case object StringInterpolation extends Exp

      case object Try extends Exp

      case object Throw extends Exp

      case object TryCatchBodyFragment extends Exp

      case object TryCatchRuleFragment extends Exp

      case object RunWithBodyExpr extends Exp

      case object RunWithRuleFragment extends Exp

      case object Tuple extends Exp

      case object TypeMatch extends Exp

      case object TypeMatchRuleFragment extends Exp

      case object Unary extends Exp

      case object UncheckedCast extends Exp

      case object Unsafe extends Exp

      case object UnsafeOld extends Exp

      case object Use extends Exp

      case object Without extends Exp

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// TYPES                                                                               //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Type extends TreeKind

    object Type {
      /**
        * A marker kind used to wrap nested types.
        */
      // For instance on a tuple type "(Int32, Bool)" you would do
      // Type
      //   Tuple
      //     Type
      //       Ident
      //     Type
      //       Ident
      case object Type extends Type

      case object Apply extends Type

      case object Argument extends Type

      case object ArgumentList extends Type

      case object Ascribe extends Type

      case object Binary extends Type

      case object CaseSet extends Type

      case object Constant extends Type

      case object Constraint extends Type

      case object ConstraintList extends Type

      case object Effect extends Type

      case object EffectSet extends Type

      case object Function extends Type

      case object PredicateWithAlias extends Type

      case object PredicateWithTypes extends Type

      case object Record extends Type

      case object RecordFieldFragment extends Type

      case object RecordRow extends Type

      case object Schema extends Type

      case object Extensible extends Type

      case object SchemaRow extends Type

      case object Tuple extends Type

      case object Unary extends Type

      case object Variable extends Type

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// PATTERNS                                                                            //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Pattern extends TreeKind

    object Pattern {
      /**
        * A marker kind used to wrap nested patterns.
        */
      // For instance on cons pattern "0 :: xs" you would do
      // Pattern
      //   FCons
      //     Pattern
      //       Literal
      //     Pattern
      //       Ident
      case object Pattern extends Pattern

      case object ExtTag extends Pattern

      case object FCons extends Pattern

      case object Literal extends Pattern

      case object Record extends Pattern

      case object RecordFieldFragment extends Pattern

      case object Tag extends Pattern

      case object Tuple extends Pattern

      case object Variable extends Pattern

      case object Unary extends Pattern

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// PREDICATES                                                                          //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Predicate extends TreeKind

    object Predicate {

      case object Atom extends Predicate

      case object Body extends Predicate

      case object Functional extends Predicate

      case object Guard extends Predicate

      case object Head extends Predicate

      case object LatticeTerm extends Predicate

      case object PatternList extends Predicate

      case object ParamList extends Predicate

      case object Param extends Predicate

      case object ParamUntyped extends Predicate

      case object TermList extends Predicate

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// IMPORTS                                                                             //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait UsesOrImports extends TreeKind

    object UsesOrImports {
      case object Alias extends UsesOrImports

      case object Import extends UsesOrImports

      case object ImportMany extends UsesOrImports

      case object Use extends UsesOrImports

      case object UseMany extends UsesOrImports

      case object UseOrImportList extends UsesOrImports
    }
  }
}
