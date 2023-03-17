//package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting
//
//import ca.uwaterloo.flix.language.ast.ErasedAst._
//import ca.uwaterloo.flix.language.ast.MonoType
//import ca.uwaterloo.flix.language.ast.Symbol._
//import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
//import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language._
//import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil._
//import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers.ErasedPrinter
//
//
//object ErasedAstPrinter {
//
//    //    exp match {
//    //      case Expression.ApplyClo(exp, args, _, _) =>
//    //        val output = applyf(doc(exp) <> metaText("clo"), args.map(a => doc(a, paren = false)))
//    //        par(output)
//    //      case Expression.ApplyDef(sym, args, _, _) =>
//    //        val output = applyf(doc(sym) <> metaText("def"), args.map(a => doc(a, paren = false)))
//    //        par(output)
//    //      case Expression.ApplyCloTail(exp, args, _, _) =>
//    //        val output = applyf(doc(exp) <> metaText("clotail"), args.map(a => doc(a, paren = false)))
//    //        par(output)
//    //      case Expression.ApplyDefTail(sym, args, _, _) =>
//    //        val output = applyf(doc(sym) <> metaText("deftail"), args.map(a => doc(a, paren = false)))
//    //        par(output)
//    //      case Expression.ApplySelfTail(sym, _, actuals, _, _) =>
//    //        val output = applyf(doc(sym) <> metaText("selftail"), actuals.map(a => doc(a, paren = false)))
//    //        par(output)
//    //      case Expression.Branch(exp, branches, tpe, loc) =>
//    //        val output = metaText("Branch")
//    //        output
//    //      case Expression.JumpTo(sym, tpe, loc) =>
//    //        val output = metaText("JumpTo")
//    //        output
//    //      case Expression.Is(sym, exp, loc) => metaText("Is")
//    //      case Expression.Untag(sym, exp, tpe, loc) => metaText("Untag")
//    //      case Expression.Index(base, offset, _, _) =>
//    //        val output = tupleIndexf(doc(base), offset)
//    //        par(output)
//    //      case Expression.Tuple(elms, _, _) =>
//    //        val output = tuplef(elms.map(doc(_, paren = false)))
//    //        output
//    //      case Expression.RecordEmpty(_, _) =>
//    //        val output = emptyRecordf()
//    //        output
//    //      case Expression.RecordSelect(exp, field, _, _) =>
//    //        val output = recordSelectf(doc(exp), text(field.name))
//    //        par(output)
//    //      case e@Expression.RecordExtend(_, _, _, _, _) =>
//    //
//    //        @tailrec
//    //        def recordDoc(exp: Expression, fields: List[(Doc, Doc)]): Doc = exp match {
//    //          case Expression.RecordExtend(field, value, rest, _, _) =>
//    //            recordDoc(rest, (text(field.name), doc(value, paren = false)) :: fields)
//    //          case Expression.RecordEmpty(_, _) =>
//    //            recordExtendf(fields.reverse, None)
//    //          case other =>
//    //            recordExtendf(fields.reverse, Some(doc(other, paren = false)))
//    //        }
//    //
//    //        recordDoc(e, Nil)
//    //      case Expression.RecordRestrict(field, rest, tpe, loc) =>
//    //        val output = metaText("RecordRestrict")
//    //        output
//    //      case Expression.ArrayLit(elms, _, _) =>
//    //        val output = arrayListf(elms.map(doc(_, paren = false)))
//    //        output
//    //      case Expression.ArrayNew(elm, len, tpe, loc) => metaText("ArrayNew")
//    //      case Expression.ArrayLoad(base, index, tpe, loc) => metaText("ArrayLoad")
//    //      case Expression.ArrayStore(base, index, elm, tpe, loc) => metaText("ArrayStore")
//    //      case Expression.ArrayLength(base, tpe, loc) => metaText("ArrayLength")
//    //      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => metaText("ArraySlice")
//    //      case Expression.Assign(exp1, exp2, _, _) =>
//    //        val output = assignf(doc(exp1), doc(exp2))
//    //        par(output)
//    //      case Expression.TryCatch(exp, rules, tpe, loc) => metaText("TryCatch")
//    //      case Expression.GetField(field, exp, tpe, loc) => metaText("GetField")
//    //      case Expression.PutField(field, exp1, exp2, tpe, loc) => metaText("PutField")
//    //      case Expression.GetStaticField(field, tpe, loc) => metaText("GetStaticField")
//    //      case Expression.PutStaticField(field, exp, tpe, loc) => metaText("PutStaticField")
//    //      case Expression.NewObject(name, clazz, tpe, methods, loc) => metaText("NewObject")
//    //      case Expression.Spawn(exp1, exp2, _, _) =>
//    //        val output = spawnf(doc(exp1, paren = false), doc(exp2))
//    //        par(output)
//    //      case _ => text("unknown")
//    //    }
//  }
//
//}
