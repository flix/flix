package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.Range

/**
  * Represents a completion context.
  *
  * @param uri          Source file URI (from client)
  * @param range        Start and end position of the word underneath (or alongside) the cursor
  * @param word         The word underneath (or alongside) the cursor
  * @param previousWord The word before the above (note that this may be on either the current or previous line)
  * @param prefix       The text from the start of the line up to the cursor
  */
case class CompletionContext(uri: String, range: Range, word: String, previousWord: String, prefix: String)
