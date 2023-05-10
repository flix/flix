# Restrictable Variants Artifact

- Title of the submitted paper:
  - Restrictable Variants: A Simple and Practical Alternative to Extensible
    Variants
- ECOOP submission number for the paper:
  - #102

--------------------------------------------------------------------------------

## Overview: What does the artifact comprise?

This artifact can either be run locally using `demo.zip` or on a VM with
`variants.OVA`. The artifact contains the following:

| File           | Explanation                                                                                              |
|:---------------|:---------------------------------------------------------------------------------------------------------|
| `README.md`    | This file, explaining this artifact                                                                      |
| `demo.zip`     | A Flix project that can be run locally in VSCode with the dependencies listed in Artifact Requirements   |
| `variants.ova` | A VM image with the preinstalled requirements and the project in `demo.zip` already set up               |

The `demo` Flix project has three flix files that represent case studies and
examples from the paper:

| Artifact File               | Paper Reference                               |
|:----------------------------|:----------------------------------------------|
| `README.md`                 | -                                             |
| `flix.jar`                  | -                                             |
| `src/colors.flix`           | _2 Motivation_ (& running example)            |
| `src/boolean-formulas.flix` | _6.1 Boolean Expressions_ (& running example) |
| `src/sequences.flix`        | _6.2 Option, List, and NonEmptyList_          |

Each file is separate and contains a main function that shows some example usage
and can be run via the VSCode button shown above the function. The files are
meant to be experimented on, either via the main function or modification to the
other functions. Types and variant index bounds can be observed by hovering the
mouse on expressions in a error-free program. The differences between the paper
and the code is explained in the
[Differences with Paper](#differences-with-paper) section.

The artifact demonstrates the usefulness of restrictable variants in a general
purpose programming language and shows the expressiveness of the type system on
a multitude of functions.

--------------------------------------------------------------------------------

## Artifact Requirements

There are two ways to run the artifact,
either through the VM or running on a local machine.
The VM bundles all dependencies but is heavyweight.
Running local requires additional dependencies but does not have the weight of a VM.


### Option 1: Virtual Machine

- VirtualBox or another virtual machine software capable of running OVA files.
- The machine must be configured to use at least two CPU cores

### Option 2: Local Installation
- Java 11+ ([e.g. openjdk](https://jdk.java.net/java-se-ri/11))
- [VSCode](https://code.visualstudio.com/download)
  - [Flix Extension](https://marketplace.visualstudio.com/items?itemName=flix.flix)
- At least two CPU-cores (because of a bug in the Flix compiler)

--------------------------------------------------------------------------------

## Getting Started

### Virtual Machine
1. Run the VM
2. In case the user is logged out:
  - Username: `flix`
  - Password: `flix`
3. Open VSCode (icon on the left)
4. Open e.g. `src/colors.flix`.
5. The Flix compiler should automatically start, using the jar in the project
   folder. Highlighting should now appear.

### Locally
1. Open VSCode.
2. Choose `File -> Open Folder` and select the unzipped `demo.zip` folder.
3. Open e.g. `src/colors.flix`.
4. The Flix compiler should automatically start, using the jar in the project
   folder. Highlighting should now appear.

### Flix Version Message
The extension with report that Flix version x.y.z. has started, but
this is not version x.y.z, but rather a custom-built version of Flix for the
purpose of artifact evaluation.

### Timeouts
The extension may occasionally time out, especially due to the limitations of VM memory.
The window can be reloaded by pressing `F1` and selecting "Developer: Reload Window",
or simply by restarting VSCode.


--------------------------------------------------------------------------------

## Differences with Paper

The implementation differs from the paper in a few ways. These are differences
are mostly un-important and simply due to engineering. We outline them below:

### Syntax

- The paper uses syntax such as `s + Cst`, `s - {Cst, Var}`, and `s + {Cst}` for
  type-level set formulas . The implementation syntax is
  `s ++ <Cst>`, `s -- <Cst, Var>`, and `s ++ <Cst>`.

- The implementation requires labels to be prefixed by their enum. Thus the
  above syntax should be `s ++ <Expr.Cst>`, `s -- <Expr.Cst, Expr.Var>`, and
  `s ++ <Expr.Cst>`.

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

### Type Inference

We have experimented with different implementation strategies for the
declarative type system presented in the paper. While the type rules in Figure 3
can readily be translated into Scala code they sometimes produce large unifiers
which impacts performance a lot currently. In the future, we would like to try
to minimize such unifiers and thus support the paper typing rules as is. In the
interim, we have experimented with an alternative implementation which
significantly reduces the size of the computed unifiers.

The implementation has alternative typing rules for tags, redefines how `open`
is used and introduces the `open_as` operator. These changes are inconsequential
for the expressiveness of the language but help the performance of type
inference by reducing the number of type variables. The inference rules are
given as typing rules to help understand their differences. These constructs can
very easily be de-sugared into terms of the original system and thus contain no
conceptual changes.

#### Typing Tags
In the paper, the type of `Color.Red` is `Color[{Red} + s]` where `s` is some
variable. In the implementation, the type of `Color.Red` is
`Color[<Color.Red>]`, i.e. the index is exactly the constructed tag.

```
Γ ⊢ e : t
Σ(E.l) ⊑ t -> E[ {E.l} ]
-------------------------
Γ ⊢ E.l(e) : E[ {E.l} ]
```

#### Typing Open Tags
You can "open" a tag constructor (not a general expression), and the type of
such an expression, `open Color.Red`, is `Color[{Red + s}]`. In some cases, we
can use `Color.Red` instead of `open Color.Red` to infer simpler types.

```
Γ ⊢ e : t
Σ(E.l) ⊑ t -> E[ phi ]
----------------------------
Γ ⊢ open E.l(e) : E[ phi ∪ {E.l} ]
```

#### Typing Open-As
This expression corresponds to `open` in the paper, but the kind of the enum in
question needs to be annotated, so `open_as Color (x: Color[<Color.red>])` has
type `Color[s ++ <Color.red>]`.

```
Γ ⊢ e : E[ phi ]
phi <: phi'
----------------------------
Γ ⊢ open_as E e : E[ phi' ]
```

#### Recursive Enums
Since `Tag(x)` requires the exact label of the constructor, `open Tag(x)` should
always be used for recursive tags. Note that `open Tag(x)` does not open the
index if the inner tag is not open, which can be seen in the typing rules below.
This means that `open Exp.Not(Exp.Cst(true))` has type `Exp[<Exp.Not, Exp.Cst>]`
while `open Exp.Not(open Exp.Cst(true))` has type `Exp[s ++ <Exp.Not, Exp.Cst>]`.
