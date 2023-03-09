# Restrictable Variants Artifact

## Getting Started

1. Open VSCode.
2. Choose `File -> Open Folder` and select this folder.
3. Open e.g. `boolean-formulas.flix`.
4. The Flix compiler should automatically start.

Note: The extension with report that Flix version x.y.z. has started, but
(obviously) this is not version x.y.z, but rather a custom built version of Flix
for the purpose of artifact evaluation.

## Differences with Paper

The implementation differs from the paper in a few ways. These are differences
are mostly un-important and simply due to engineering. We outline them below:

- The paper uses syntax such as `s + Cst`, `s - {Cst, Var}`, and `s + {Cst}` for
  type-level set formulas . The implementation syntax is `s ++ <Cst>`, `s --
  <Cst, Var>`, and `s ++ <Cst>`.

- The implementation requires labels to be prefixed by their enum. Thus the
  above syntax should be `s ++ <Expr.Cst>`, `s -- <Expr.Cst, Expr.Var>`, and `s
  ++ <Expr.Cst>`.

- Flix requires signatures for top-level functions. Given the function:

```flix
def isWarm(c: Color[<Color.Red, Color.Blue>]): Bool = 
    choose c {
        case Color.Red  => true 
        case Color.Blue => false
    }
```

to see its inferred signature, write a program:

```flix
def main(): Unit = 
    def _isWarm(c) = 
        choose c {
            case Color.Red  => true 
            case Color.Blue => false
        };
    ()
```

and however on the `_isWarm` token. Note that local functions like `_isWarm` are
not let-generalized in Flix.

- Flix has alternative typing rules for tags, redefines how `open` is used and 
  introduces the `open_as` operator. These changes are inconsequential for the 
  expressiveness of the language but help the performance of type inference by
  reducing the number of type variables.

- Typing tags: In the paper, the type of `Color.Red` is `Color[{Red} + s]` where
  `s` is some variable. In the implementation, the type of `Color.Red` is
  `Color[<Color.Red>]`, i.e. the index is exactly the constructed tag.

- Typing open tags: You can "open" a tag constructor (not a general expression),
  and the type of such an expression, `open Color.Red`, is `Color[{Red + s}]`.
  In some cases, we can use `Color.Red` instead of `open Color.Red` to infer
  simpler types.

- Typing `open_as`: This expression corresponds to `open` in the paper, but the
  kind of the enum in question needs to be annotated, so
  `open_as Color (x: Color[<Color.red>])` has type `Color[s ++ <Color.red>]`.

- Recursive enums: since `Tag(x)` requires the exact label of the constructor,
  `open Tag(x)` should always be used for recursive tags. Note that
  `open Tag(x)` does not open the index if the inner tag is not open, which can
  be seen from the typing rule below. This means that
  `open Exp.Not(Exp.Cst(true))` has type `Exp[<Exp.Not, Exp.Cst>]` while
  `open Exp.Not(open Exp.Cst(true))` has type `Exp[s ++ <Exp.Not, Exp.Cst>]`.


### Implementation Typing Rules

These rules replace T-Tag and T-Open from the paper. Note again that these
modifications are not required or change the expressiveness of the language and
are merely slight changes to improve inference performance. These constructs can
very easily be de-sugared into terms of the original system.

#### T-Tag
```
Γ ⊢ e : t
Σ(E.l) ⊑ t -> E[ {E.l} ]
-------------------------
Γ ⊢ E.l(e) : E[ {E.l} ]
```
#### T-Open-Tag
```
Γ ⊢ e : t
Σ(E.l) ⊑ t -> E[ phi ]
----------------------------
Γ ⊢ open E.l(e) : E[ phi ∪ {E.l} ]
```
#### T-Open-As
```
Γ ⊢ e : E[ phi ]
phi <: phi'
----------------------------
Γ ⊢ open_as E e : E[ phi' ]
```

