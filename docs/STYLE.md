# Style Guide

## Both

- Prefer functional to imperative programming.
- Use private functions/methods.

## Flix

- Variable names are typical one letter; `o` for Option, l` for `List`.
- Effect variables are called `ef` or `ef1`, `ef2`...

## Scala

- No shadowed variables.
- No unused local variables.
- Common methods are `visitExp`, `visitExps`, `visitPat`, etc.
- Prefer to name expressions just `exp1`, `exp2`, `exp3`.
  - Names such as `beginExp` etc. quickly get outdated.
- Leave the code in better state than you found it in.

If a PR discovers a new style principle, feel free to add it to this file as part of the same PR.
