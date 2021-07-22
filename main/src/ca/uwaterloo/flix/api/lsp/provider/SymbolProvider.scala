package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{Location, Position, SymbolInformation, SymbolKind, Range}

object SymbolProvider {

  def getSymbol(uri: String, pos: Position): SymbolInformation =
    {
      val start:Position = Position(1, 0);
      val end:Position = Position(5, 0);
      val r = Range(start, end);
      var l = Location(uri, r);
//      List(
        SymbolInformation("manoj", SymbolKind.Function, l, "my container")
//      )
    }
}
