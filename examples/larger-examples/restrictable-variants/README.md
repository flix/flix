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

- The Flix case studies occasionally use the `open` expression. In the paper,
  type of `Color.Red` is `Color[{Red} + s]` where `s` is some variable. In the
  implementation, the type of `Color.Red` is `Color[{Red}]`, but the type of
  `open Color.Red` is `Color[{Red + s}]`. In some cases, we can use `Color.Red`
  instead of `open Color.Red` to infer simpler types. 
