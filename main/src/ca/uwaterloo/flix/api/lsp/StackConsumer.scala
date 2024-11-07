/*
 * Copyright 2024 Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.Visitor.Consumer
import ca.uwaterloo.flix.language.ast.Ast.{AssocTypeConstructor, Derivation, Derivations}
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record.RecordLabelPattern
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.language.ast.shared.{Annotation, EqualityConstraint, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Name, Symbol, Type}

/**
  * Consumer that collects every visited AST nodes on a stack where the head is the last element visited.
  *
  * Whenever `consumeX` is called for some AST node `x: X` by the [[Visitor]], `x` is pushed onto the stack.
  * This way, after the visitor is done, every AST node visited is accessible via [[getStack]].
  */
case class StackConsumer() extends Consumer {
  private var stack: List[AnyRef] = Nil

  private def push(x: AnyRef): Unit = {
    stack = x :: stack
  }

  /**
    * @return The stack of AST elements that the consumer has collected.
    */
  def getStack: List[AnyRef] = stack

  override def consumeAnnotation(ann: Annotation): Unit = push(ann)
  override def consumeAssocTypeConstructor(tcst: AssocTypeConstructor): Unit = push(tcst)
  override def consumeAssocTypeDef(tdefn: AssocTypeDef): Unit = push(tdefn)
  override def consumeAssocTypeSig(tsig: AssocTypeSig): Unit = push(tsig)
  override def consumeAssocTypeSymUse(symUse: AssocTypeSymUse): Unit = push(symUse)
  override def consumeBinder(bnd: Binder): Unit = push(bnd)
  override def consumeCase(cse: Case): Unit = push(cse)
  override def consumeCaseSymUse(sym: CaseSymUse): Unit = push(sym)
  override def consumeCatchRule(rule: CatchRule): Unit = push(rule)
  override def consumeConstraint(c: Constraint): Unit = push(c)
  override def consumeConstraintParam(cparam: ConstraintParam): Unit = push(cparam)
  override def consumeDef(defn: Def): Unit = push(defn)
  override def consumeDefSymUse(sym: DefSymUse): Unit = push(sym)
  override def consumeDerivation(derive: Derivation): Unit = push(derive)
  override def consumeDerivations(derives: Derivations): Unit = push(derives)
  override def consumeEff(eff: Effect): Unit = push(eff)
  override def consumeEffectSymUse(effUse: EffectSymUse): Unit = push(effUse)
  override def consumeEnum(enm: Enum): Unit = push(enm)
  override def consumeEqualityConstraint(ec: EqualityConstraint): Unit = push(ec)
  override def consumeExpr(exp: Expr): Unit = push(exp)
  override def consumeFormalParam(fparam: FormalParam): Unit = push(fparam)
  override def consumeParYieldFragment(frag: ParYieldFragment): Unit = push(frag)
  override def consumeHandlerRule(rule: HandlerRule): Unit = push(rule)
  override def consumeInstance(ins: Instance): Unit = push(ins)
  override def consumeJvmMethod(method: JvmMethod): Unit = push(method)
  override def consumeLabel(label: Name.Label): Unit = push(label)
  override def consumeLocalDefSym(symUse: LocalDefSymUse): Unit = push(symUse)
  override def consumeMatchRule(rule: MatchRule): Unit = push(rule)
  override def consumeOp(op: Op): Unit = push(op)
  override def consumeOpSymUse(sym: OpSymUse): Unit = push(sym)
  override def consumePattern(pat: Pattern): Unit = push(pat)
  override def consumePredicate(p: Predicate): Unit = push(p)
  override def consumePredicateParam(pparam: PredicateParam): Unit = push(pparam)
  override def consumeRecordLabelPattern(pat: RecordLabelPattern): Unit = push(pat)
  override def consumeSelectChannelRule(rule: SelectChannelRule): Unit = push(rule)
  override def consumeSig(sig: Sig): Unit = push(sig)
  override def consumeSigSymUse(symUse: SigSymUse): Unit = push(symUse)
  override def consumeStruct(struct: Struct): Unit = push(struct)
  override def consumeStructField(field: StructField): Unit = push(field)
  override def consumeStructFieldSymUse(symUse: StructFieldSymUse): Unit = push(symUse)
  override def consumeTypeMatchRule(rule: TypeMatchRule): Unit = push(rule)
  override def consumeTrait(traitt: Trait): Unit = push(traitt)
  override def consumeTraitConstraint(tc: TraitConstraint): Unit = push(tc)
  override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = push(tcHead)
  override def consumeTraitSymUse(symUse: TraitSymUse): Unit= push(symUse)
  override def consumeType(tpe: Type): Unit = push(tpe)
  override def consumeTypeAlias(alias: TypeAlias): Unit = push(alias)
  override def consumeTypeParam(tparam: TypeParam): Unit = push(tparam)
  override def consumeVarBinder(varSym: Symbol.VarSym, tpe: Type): Unit = push((varSym, tpe))
}
