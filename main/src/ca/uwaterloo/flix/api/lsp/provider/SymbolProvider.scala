package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{Location, Position, Range, SymbolInformation, SymbolKind}
import ca.uwaterloo.flix.language.ast.TypedAst.Root

object SymbolProvider {

  def getSymbolList(root: Root): List[SymbolInformation] =
    {
//      val start:Position = Position(1, 0);
//      val end:Position = Position(5, 0);
//      val r = Range(start, end);
//      val l = Location("hello.flix", r);
//
      root.defs.map({
        case (sym, defn) => SymbolInformation(sym.name, SymbolKind.Function, Location.from(sym.loc), "my_container")
      }).toList
    }
}
