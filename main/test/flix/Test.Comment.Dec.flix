mod Test.Comment.Dec {

    import java.lang.Object
    // a line comment between two imports
    import java.lang.Runnable
    /* a block comment between two imports */
    import java.lang.Thread

    pub def imports01(o: Object, r: Runnable, t: Thread): (Object, Runnable, Thread) = (o, r, t)

    /// Doc line one
    // a line comment inside a doc comment
    /* a block comment inside a doc comment */
    /// Doc line two
    pub /* c */ def docBlock01(): Int32 = 1

    /// Doc
    @Lazy // trailing after an annotation
    pub def annotation01(): Int32 = 1

    ///
    /// Doc with an empty first and last line
    ///
    pub def docEmptyLines01(): Int32 = 1

    // a line comment between two declarations

    /* a block comment between two declarations */

    ///
    /// Doc after plain comments between declarations
    ///
    pub def docEmptyLines02(): Int32 = 2

    /*
    pub def commentedOut(x: Int32): Int32 =
        /* a block comment nested inside a block comment */
        x + 1
    */

    mod M {
        // only a comment in this module
    }

    pub trait Size[a] {
        /// doc on a signature
        pub def size(x: a): Int32
        // trailing comment in a trait
    }

    pub enum Color {
        /// doc on Red
        case Red, // trailing after Red
        // line comment before Green
        case Green,
        /// doc on Blue after a comma
        case Blue // trailing after Blue
    }

    pub struct Point[r] {
        // comment before field
        x: Int32, // trailing field comment
        /// doc on field
        y: Int32
    }

    ///
    /// Doc on a module
    ///
    mod Docs {

        /// Doc on a type alias
        pub type alias Alias = Int32

        /// Doc on an enum
        pub enum Shape with Eq {
            case Circle(Int32)
        }

        /// Doc on a restrictable enum
        pub restrictable enum Expr[_] {
            /// Doc on a restrictable case
            case Cst
        }

        /// Doc on a struct
        pub struct Pair[r] {
            /// Doc on a mutable field
            mut fst: Int32,
            snd: Int32
        }

        /// Doc on an effect
        pub eff Logger {
            /// Doc on an op
            def log(s: String): Unit
        }

        /// Doc on a sealed trait
        pub sealed trait Sealed[a]

        /// Doc on a trait
        pub trait Container[a] {
            /// Doc on an associated type signature
            type Elem[a]: Type

            /// Doc on a signature
            pub def first(x: a): Option[Container.Elem[a]]

            /// Doc on a signature with a default implementation
            pub def isEmpty(x: a): Bool = Option.isEmpty(Container.first(x))
        }

        /// Doc on an instance
        instance Container[String] {
            /// Doc on an associated type definition
            type Elem[String] = Char

            /// Doc on a def in an instance
            pub def first(x: String): Option[Char] = List.head(String.toList(x))

            /// Doc on a redef
            redef isEmpty(x: String): Bool = String.isEmpty(x)
        }

        /// Doc on a nested module
        mod Inner {
            /// Doc on a def in a nested module
            pub def f(): Int32 = 1
        }

    }

    // look-ahead in declaration headers
    pub enum Payload {
        case /* c */ A /* c */ (Int32), /* c */ B
    }
    pub def header01 /* c */ (x: a): a // c
        with /* c */ Eq[a] = x
    pub def header02(): Int32 \ /* c */ {} // c
        = 1

}

// trailing comment at the end of the file
