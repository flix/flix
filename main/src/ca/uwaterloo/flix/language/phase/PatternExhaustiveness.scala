/*
 * Copyright 2017 Jason Mittertreiner
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.ast.{Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern
import ca.uwaterloo.flix.language.errors.ExhaustiveMatchError
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The Pattern Exhaustiveness phase checks pattern matches for exhaustiveness
  * as well as for useless patterns
  *
  * A pattern is useless if:
  * A pattern match is exhaustive if:
  */
object PatternExhaustiveness extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Returns an error message if a pattern match is not exhaustive
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = {
    if (flix.options.debug) {
      // Can probably do options stuff here
    }
    implicit val _ = flix.genSym
    val defns = root.definitions.map { case (k, v) => k -> Definition.CheckPats(root, v) }

    root.toSuccess
  }

  object Definition {
    def CheckPats(root: TypedAst.Root, tast: TypedAst.Declaration.BoundedLattice)(implicit genSym: GenSym): Validation[TypedAst.Declaration.BoundedLattice, CompilationError] = tast match {
      case TypedAst.Declaration.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) =>
        for {
          _bot <- Expression.CheckPats(root, bot)
          _top <- Expression.CheckPats(root, top)
          _leq <- Expression.CheckPats(root, leq)
          _lub <- Expression.CheckPats(root, lub)
          _glb <- Expression.CheckPats(root, glb)
        } yield TypedAst.Declaration.BoundedLattice(tpe, _bot, _top, _leq, _lub, _glb, loc)
    }
    def CheckPats(root: TypedAst.Root, tast: TypedAst.Declaration.Definition)(implicit genSym: GenSym): Validation[TypedAst.Declaration.Definition, CompilationError] = tast match {
      case TypedAst.Declaration.Definition(doc, ann, sym, tparams, fparams, exp, tpe, loc)  =>
        for {
        _exp <- Expression.CheckPats(root, exp)
        } yield TypedAst.Declaration.Definition(doc, ann, sym, tparams, fparams, _exp, tpe, loc)
    }

    def CheckPats(root: TypedAst.Root, tast: TypedAst.Declaration.Index)(implicit genSym: GenSym): TypedAst.Declaration.Index = tast
  }

  object Expression {
    def CheckPats(root: TypedAst.Root, tast: TypedAst.Expression)(implicit genSym: GenSym): Validation[TypedAst.Expression, CompilationError] = tast match {
      case TypedAst.Expression.Match(exp, rules, tpe, loc) => {
        for  {
          _rules <- CheckRules(root, exp, rules)
        } yield TypedAst.Expression.Match(exp, _rules, tpe, loc)
      }


      case a => Success(a, Stream.empty)
    }

    def CheckRules(root: TypedAst.Root, exp: TypedAst.Expression, rules: List[TypedAst.MatchRule]): Validation[List[TypedAst.MatchRule], CompilationError] = {
      AlgI(root, exp, rules.map(r => List(r.pat)), 1) match {
        case Right(a) => Success(rules, Stream.empty)
        case Left((c, rule)) => Failure(ExhaustiveMatchError(rule.toString(), rule.head.loc.source) #:: Stream.empty);
      }
    }

    /**
      * Given a list of patterns, computesa pattern vector of size n such
      * that all the instances of p are no matching values
      *
      * If no such pattern exists, returns the input
      */
    def AlgI(root: TypedAst.Root, exp: TypedAst.Expression, rules: List[List[TypedAst.Pattern]], n: Integer): Either[(EnumSym, List[Pattern]), List[List[Pattern]]] ={
      if (rules.isEmpty && n==0) {
        return Right(rules)
      }

      val ak = 0
      val sigma = rootCtors(rules)
      if (completeSig(root, exp, sigma)) {
        val check_all: List[Either[(EnumSym, List[Pattern]), List[List[Pattern]]]] = sigma.map(c => AlgI(root, exp, AlgS(root, c, rules), ak + n - 1))
        val base: Either[(EnumSym, List[Pattern]), List[List[Pattern]]] =
          Right(List.empty[List[Pattern]])
        val rules_valid: Either[(EnumSym, List[Pattern]), List[List[Pattern]]] =
          check_all.foldRight(base)(mergeEither)
        rules_valid match {
          case Right(a) => Right(a)
          case Left((c, pats)) => Left((c, List(TypedAst.Pattern.Tag(c, "TEST", TypedAst.Pattern.Tuple(pats, exp.tpe, exp.loc), exp.tpe, exp.loc))))
        }
      } else {
        AlgI(root, exp, AlgD(rules), n - 1) match {
          case Right(a) => Right(a)
          case Left((c, pats)) => sigma match {
            case Nil => Left(c, Pattern.Wild(exp.tpe, exp.loc) :: pats)
            case _ => Left(c, List(TypedAst.Pattern.Tag(c, "TEST", TypedAst.Pattern.Tuple(pats, exp.tpe, exp.loc), exp.tpe, exp.loc)))
          }
        }
      }
    }

    // Specialize a matrix of patterns for the Constructor ctor
    // For a constructor of C(r1,...ra) and a matrix of width n,
    // we return a matrix of width n+a-1
    def AlgS(root: TypedAst.Root, ctor: EnumSym, rules: List[List[TypedAst.Pattern]]): List[List[TypedAst.Pattern]] = {
      // First figure out how many arguments are needed by the ctor
      val numArgs = root.enums.get(ctor).get.cases.size

      val specializeRow = (pat: List[TypedAst.Pattern], acc: List[List[TypedAst.Pattern]]) =>
        pat.head match {
          // If it's a pattern with the constructor that we are
          // specializing for, we break it up into it's arguments
          // If it's not our constructor, we ignore it
          case TypedAst.Pattern.Tag(sym, _, exp, _, _) => exp match {
            case TypedAst.Pattern.Tuple(elms, _, _) =>
              if (sym == ctor) {
                (elms ++ pat.tail) :: acc
              } else acc
            case _ => ???
          }
          // A wild constructor is the same as the constructor
          // with all its arguments as wild
          case a: TypedAst.Pattern.Wild =>
            (List.fill(numArgs)(a) ++ pat.tail) :: acc
          // We don't have or patterns, but if we did, they would go here
        }
      rules.foldRight(List.empty[List[TypedAst.Pattern]])(specializeRow)
    }

    def AlgD(rules: List[List[TypedAst.Pattern]]): List[List[TypedAst.Pattern]] = {
      val defaultRow = (pat: List[TypedAst.Pattern], acc: List[List[TypedAst.Pattern]]) => pat.head match {
        case _: TypedAst.Pattern.Tag => acc
        case _: TypedAst.Pattern.Wild => pat.tail :: acc
        // Or pattern case would go here
      }
      rules.foldRight(List.empty[List[TypedAst.Pattern]])(defaultRow)
    }


    /**
      * Computes the set of constructors that appear at the root of the
      * patterns
      */
    def rootCtors(rules: List[List[TypedAst.Pattern]]): List[EnumSym] = {
      val rootCtor = (pat: TypedAst.Pattern, pats: List[EnumSym]) => pat match {
        case _: Pattern.Wild => pats
        case _: Pattern.Var => pats
        case t: Pattern.True => new EnumSym(List.empty, "True", t.loc) :: pats
        case f: Pattern.False => new EnumSym(List.empty, "False", f.loc) :: pats
        case u: Pattern.Unit => new EnumSym(List.empty, "Unit", u.loc) :: pats
        case tg: Pattern.Tag => tg.sym :: pats
      }
     rules.head.foldRight(List.empty[EnumSym])(rootCtor)
    }

    /**
      * True if ctors is a complete signature for exp
      * @param root Root of the expression tree
      * @param exp The expression to match
      * @param ctors The ctors that we match with
      * @return
      */
    def completeSig(root: TypedAst.Root, exp: TypedAst.Expression, ctors: List[EnumSym]): Boolean = {
      // Enumerate all the constructors that we need to cover
      val expCtors: List[String] = exp.tpe match {
        case Type.Bool => List("True", "False")
        case Type.Unit => List("Unit")
        case Type.Enum(enum,_) => root.enums.get(enum).get.cases.toList.map(_._1)
        case Type.FTuple(_) => List("Tuple")
        /* For "infinite" types, the only way we get a match is through a
         * wild. Technically, you could, for example, cover a Char by
         * having a case for [0 255], but we'll ignore that case for now
         */
        case _ => return false
      }
      /* We cover the needed constructors if there is a wild card in the
       * root constructor set, or if we match every constructor for the
       * expression
       */
      expCtors.toSet.diff(ctors.toSet.map(_.name)).isEmpty
    }


    def mergeEither(x: Either[(EnumSym, List[Pattern]), List[List[Pattern]]],
                    acc: Either[(EnumSym, List[Pattern]), List[List[Pattern]]]): Either[(EnumSym, List[Pattern]), List[List[Pattern]]] =
        (x, acc) match {
          case (_, Left((ctor, pat))) => Left((ctor, pat))
          case (Left((ctor, pat)), _) => Left((ctor, pat))
          case (Right(a), Right(b)) => Right(a ++ b)
        }
  }
}
