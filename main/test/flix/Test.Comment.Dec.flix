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

}

// trailing comment at the end of the file
