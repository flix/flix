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
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Tag
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
    //val defns = seqM(root.definitions.map { case (_, v) => Definition.CheckPats(root, v) })
    val defns1 = root.definitions.filter { case (_, v) => Definition.CheckPats(root, v).isFailure }
    val defns = seqM(defns1.map { case (_, v) => Definition.CheckPats(root, v) })

    defns.map ( _ => root)
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
      case TypedAst.Expression.Match(exp, rules, tpe, loc) =>
        for  {
          _rules <- CheckRules(root, exp, rules)
        } yield TypedAst.Expression.Match(exp, _rules, tpe, loc)


      case a => Success(a, Stream.empty)
    }

    def CheckRules(root: TypedAst.Root, exp: TypedAst.Expression, rules: List[TypedAst.MatchRule]): Validation[List[TypedAst.MatchRule], CompilationError] = {
      FindNonMatchingPat(root, exp, rules.map(r => List(r.pat)), 1) match {
        case Right(a) => Success(rules, Stream.empty)
        case Left((c, rule)) => Failure(ExhaustiveMatchError(formatRule(rule), rule.head.loc.source) #:: Stream.empty);
      }
    }

    def formatRule(rule: List[TypedAst.Pattern]): String = {
      def formatPattern(pat: TypedAst.Pattern, acc: String):String = pat match {
        case Pattern.Int8(lit, _) => acc ++ ", " ++ lit.toString
        case Pattern.Int16(lit, _) => acc ++ ", " ++ lit.toString
        case Pattern.Int32(lit, _) => acc ++ ", " ++ lit.toString
        case Pattern.Int64(lit, _) => acc ++ ", " + lit.toString
        case Pattern.Float32(lit, _) => acc ++ ", " ++ lit.toString
        case Pattern.Float64(lit, _) => acc ++ ", " ++ lit.toString
        case Pattern.Char(lit, _) => acc ++ ", " ++ lit.toString
        case Pattern.BigInt(lit, _) => acc ++ ", " ++ lit.toString
        case Pattern.Wild(_, _) => acc ++ ", _"
        case Pattern.Var(_, _, _) => acc ++ ", _"
        case Pattern.Tuple(pats, _ ,_ ) => acc ++ ", (" ++ formatRule(pats) ++ ")"
        case Pattern.Tag(sym, tag, pt, _, _) => acc ++ ", " ++ sym.name ++ "/" ++ tag ++ formatRule(List(pt))
      }
      rule.foldRight("")(formatPattern)
    }

    /**
      * Given a list of patterns, computes a pattern vector of size n such
      * that p doesn't match any rule in rules
      *
      * @param  root The AST root of the expression
      * @param  exp The expression the patterns match against
      * @param rules The rules to check for exhaustion
      * @param n The size of resulting pattern vector
      * @return If no such pattern exists, returns Right(the input), else returns Left(a matching pattern)
      */
    def FindNonMatchingPat(root: TypedAst.Root, exp: TypedAst.Expression, rules: List[List[TypedAst.Pattern]], n: Integer): Either[(EnumSym, List[Pattern]), List[List[Pattern]]] = {
      if (n==0) {
        if (rules.isEmpty) {
          return Left((new EnumSym(List.empty[String], "", null), List.empty[Pattern]))
        }
        return Right(rules)
      }

      val sigma = rootCtors(rules)
      if (completeSig(root, exp, sigma)) {
        /* If the constructors are complete, then we check that the arguments to the constructors and the remaining
         * patterns are complete
         *
         * e.g. If we have
         * enum Option {
         *    case Some a,
         *    case Nothing
         *  }
         * case Some True =>
         * case Some False =>
         * case Nothing =>
         *
         * {Some, Nothing} is a complete signature, but the just case is exhaustive only if the {True, False} case is
         *
         * exhaustive. So we create a "Specialized" matrix for Just with {True, False} as rows and check that.
         */
        val check_all: List[Either[(EnumSym, List[Pattern]), List[List[Pattern]]]] = sigma.map(c => FindNonMatchingPat(root, exp, Specialize(root, c, rules),  countCtorArgs(c) + n - 1))
        val base: Either[(EnumSym, List[Pattern]), List[List[Pattern]]] = Right(List.empty[List[Pattern]])
        val rules_valid: Either[(EnumSym, List[Pattern]), List[List[Pattern]]] = check_all.foldRight(base)(mergeEither)
        rules_valid match {
          case Right(a) => Right(a)
          case Left((c, pats)) => Left((c, List(TypedAst.Pattern.Tag(c, c.name, TypedAst.Pattern.Tuple(pats, exp.tpe, exp.loc), exp.tpe, exp.loc))))
        }
      } else {
        FindNonMatchingPat(root, exp, defaultMatrix(rules), n - 1) match {
          case Right(a) => Right(a)
          case Left((c, pats)) => sigma match {
            case Nil => Left(c, Pattern.Wild(exp.tpe, exp.loc) :: pats)
            case _ => Left(c, List(TypedAst.Pattern.Tag(c, c.name, TypedAst.Pattern.Tuple(pats, exp.tpe, exp.loc), exp.tpe, exp.loc)))
          }
        }
      }
    }

    /**
      * Specialize a matrix of patterns for the Constructor ctor For a constructor of C(r1,...ra) and a matrix of width
      * n, we return a matrix of width n+a-1
      *
      * e.g. If we have
      * enum Option {
      *    case Some a,
      *    case Nothing
      *  }
      *
      * And the pattern matrix
      *
      * case Some True =>
      * case Some False =>
      * case Nothing =>
      *
      * Specializing for Some gives use the matrix
      * True =>
      * False =>
      *
      * Where' we've taken a matrix had made of the arguments to the Some rows. Specializing for Nothing would give
      * an empty matrix as it has no arguments.
      *
      * @param root The ast root
      * @param ctor The constructor to specialize for
      * @param rules The rules matrix to specialize
      * @return The specialized matrix of rules
      */
    def Specialize(root: TypedAst.Root, ctor: TypedAst.Pattern, rules: List[List[TypedAst.Pattern]]): List[List[TypedAst.Pattern]] = {
      // First figure out how many arguments are needed by the ctor
      val numArgs = countCtorArgs(ctor)

      def specializeRow(pat: List[TypedAst.Pattern], acc: List[List[TypedAst.Pattern]]) = pat.head match {
        // A wild constructor is the same as the constructor
        // with all its arguments as wild
        case a: TypedAst.Pattern.Wild =>
          (List.fill(numArgs)(a) ++ pat.tail) :: acc
        case a: TypedAst.Pattern.Var =>
          (List.fill(numArgs)(a) ++ pat.tail) :: acc

        // If it's a pattern with the constructor that we are
        // specializing for, we break it up into it's arguments
        // If it's not our constructor, we ignore it
        case TypedAst.Pattern.Tag(_, tag, exp, _, _) => exp match {
          case TypedAst.Pattern.Tuple(elms, _, _) =>
            if (tag.equals(patName(ctor))) {
              (elms ++ pat.tail) :: acc
            } else acc
          case TypedAst.Pattern.Unit(_) =>
            if (tag.equals (patName (ctor) ) ) {
              pat.tail :: acc
            } else acc
          case _ =>
            if (tag.equals (patName (ctor) ) ) {
              (exp :: pat.tail) :: acc
            } else acc
        }

        // Also handle the non tag constructors
        case p =>
          if (patName(p).equals(patName(ctor))) {
            (p :: pat.tail) :: acc
          } else acc
      }

      rules.foldRight(List.empty[List[TypedAst.Pattern]])(specializeRow)
    }

    /**
      * Extract a default matrix of width n-1
      *
      * DefaultMatrix is called we called FindNonMatchingPat when the given constructor patterns don't cover every
      * possibility. We want to check if we take one of the wild card patterns, it is exhaustive.
      *
      * DefaultMatrix constructs a matrix which only has the patterns that begin with a wildcard, then removes the
      * first wildcard since we know all the patterns start with it.
      *
      * Similar to calling `tail` on a list
      */
    def defaultMatrix(rules: List[List[TypedAst.Pattern]]): List[List[TypedAst.Pattern]] = {
      val defaultRow = (pat: List[TypedAst.Pattern], acc: List[List[TypedAst.Pattern]]) => pat.head match {
        // If it's a wild card, we take the rest of the pattern
        case _: TypedAst.Pattern.Wild => pat.tail :: acc
        case _: TypedAst.Pattern.Var => pat.tail :: acc

        // If it's a constructor, we don't include a row
        case _ => acc
      }
      rules.foldRight(List.empty[List[TypedAst.Pattern]])(defaultRow)
    }


    /**
      * Computes the set of constructors that appear at the root of the
      * patterns
      */
    def rootCtors(rules: List[List[TypedAst.Pattern]]): List[TypedAst.Pattern] = {
      def rootCtor(pat: List[TypedAst.Pattern], pats: List[TypedAst.Pattern]) = pat.head match {
        case _: Pattern.Wild => pats
        case _: Pattern.Var => pats
        case tg: Pattern.Tag => tg :: pats
        case p => p :: pats
      }
     rules.foldRight(List.empty[TypedAst.Pattern])(rootCtor)
    }

    /**
      * True if ctors is a complete signature for exp. A complete signature is when all constructors of a type are
      * present. E.g. for
      *
      * enum Color {
      *    case Red,
      *    case Blue,
      *    case Yellow
      *  }
      *
      * {Red, Blue, Yellow} is a complete signature, but {Red, Blue} is not. Additionally, {Red, Blue, _} is also not
      * If the constructors are a complete signature, then they are exhaustive for the type, and we just have to
      * check that their arguments are also exhaustive
      *
      * Wildcards are exhaustive, but we need to do some additional checking in that case (@see DefaultMatrix)
      *
      * @param root Root of the expression tree
      * @param exp The expression to match
      * @param ctors The ctors that we match with
      * @return
      */
    def completeSig(root: TypedAst.Root, exp: TypedAst.Expression, ctors: List[Pattern]): Boolean = {
      // Enumerate all the constructors that we need to cover
      val expCtors: List[String] = exp.tpe match {
        case Type.Bool => List("True", "False")
        case Type.Unit => List("Unit")
        case Type.Enum(enum,_) => root.enums.get(enum).get.cases.toList.map(_._1)
        case Type.FTuple(_) => List("Tuple")
        /* For numeric types, we consider them as "infinite" types union
         * Int = ...| -1 | 0 | 1 | 2 | 3 | ...
         * The only way we get a match is through a wild. Technically, you could, for example, cover a Char by
         * having a case for [0 255], but we'll ignore that case for now
         */
        case p => return false
      }
      /* We cover the needed constructors if there is a wild card in the
       * root constructor set, or if we match every constructor for the
       * expression
       */
      val ctornames = ctors.map {
        case TypedAst.Pattern.Tag(_, tg, _, _, _) => tg
        case p => patName(p)
      }
      expCtors.toSet.diff(ctornames.toSet).isEmpty
    }

    /**
      * Gets the number of arguments for a constructor, takes into account the "fake constructors"
      *
      * @param ctor The constructor to get from
      * @return The number of arguments for the constructor
      */
    def countCtorArgs(ctor: Pattern): Int = ctor match {
      case _:TypedAst.Pattern.True => 0
      case _:TypedAst.Pattern.False => 0
      case _:TypedAst.Pattern.Unit => 0
      case _:TypedAst.Pattern.BigInt => 1
      case _:TypedAst.Pattern.Int8 => 1
      case _:TypedAst.Pattern.Int16 => 1
      case _:TypedAst.Pattern.Int32 => 1
      case _:TypedAst.Pattern.Int64 => 1
      case _:TypedAst.Pattern.Float32 => 1
      case _:TypedAst.Pattern.Float64 => 1
      case _:TypedAst.Pattern.Str => 1
      case _:TypedAst.Pattern.Char => 1
      case _: TypedAst.Pattern.Wild => 0
      case _: TypedAst.Pattern.Var => 0
      case TypedAst.Pattern.Tuple(elems, _ ,_ ) => elems.size
      case TypedAst.Pattern.Tag(_, _,pat, _ ,_) => countCtorArgs(pat)
    }

    def patName(p: TypedAst.Pattern): String = p match {
      case _: Pattern.True => "True"
      case _: Pattern.False => "False"
      case _: Pattern.Unit => "Unit"
      case _: Pattern.Tuple => "Tuple"
      case _: Pattern.BigInt => "BigInt"
      case _: Pattern.Int8 => "Int8"
      case _: Pattern.Int16 => "Int16"
      case _: Pattern.Int32 => "Int32"
      case _: Pattern.Int64 => "Int64"
      case _: Pattern.Float32 => "Float32"
      case _: Pattern.Float64 => "Float64"
      case _: Pattern.Str => "Str"
      case _: Pattern.Char => "Char"
      case _: Pattern.Wild => "Wild"
      case _: Pattern.Var => "Var"
      case p: Pattern.Tag => p.tag
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
