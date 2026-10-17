mod Test.Comment.Dec {

    /// Doc line one
    // a line comment inside a doc comment
    /* a block comment inside a doc comment */
    /// Doc line two
    pub /* c */ def docBlock01(): Int32 = 1

    /// Doc
    @Lazy // trailing after an annotation
    pub def annotation01(): Int32 = 1

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
