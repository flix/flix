# Style Guide

## Flix and Scala

- Every file must start with a copyright header.
- Prefer functional to imperative programming.
  - Use of local mutability is okay.
- If a function or method can be private, make it private.

## Flix-specific

- Variable names are typical one letter; `o` for Option, `l` for `List`.
- Type variable names are typically `a`, `b`, `c`.
- Effect variables are called `ef` or `ef1`, `ef2`...
- Type class instances declarations should appear just below a type declaration.
  - Instances should appear in the order: Eq, Order, ToString.
- Avoids casts.
  - If necessary, effect casts are OK.
  - Only in extreme cases are type casts OK.

## Scala-specific

- No shadowed variables.
- No unused local variables.
- Common methods are `visitExp`, `visitExps`, `visitPat`, etc.
- Prefer to name expressions just `exp1`, `exp2`, `exp3`.
  - Names such as `beginExp` etc. quickly get outdated.
- Leave the code in better state than you found it in.

If a PR discovers a new style principle, feel free to add it to this file as part of the same PR.
