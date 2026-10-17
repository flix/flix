mod Test.Comment.Exp {

    // if-then-else
    pub def ifThenElse01(): Int32 = if /* c */ (true) 1 else 2
    pub def ifThenElse02(): Int32 = if (true) /* c */ 1 else 2
    pub def ifThenElse03(): Int32 = if (true) 1 /* c */ else 2
    pub def ifThenElse04(): Int32 = if (true) 1 else /* c */ 2

    // lambda
    pub def lambda01(): Int32 -> Int32 = x /* c */ -> x
    pub def lambda02(): Int32 -> Int32 = x -> /* c */ x
    pub def lambda03(): Int32 -> Int32 = /* c */ x -> x

    // let
    pub def let01(): Int32 =
        let /* c */ x = 1; // trailing
        // leading
        x

    // parameters
    pub def parameters01(x: Int32 /* c */, /* c */ y: Int32, z /* c */ : /* c */ Int32): (Int32, Int32, Int32) = (x, y, z)

    // match
    pub def match01(x: Int32): Int32 = match x {
        // leading
        case 1 => /* c */ 1
        /* between */
        case _ /* c */ => 2
        // trailing
    }

    // block
    pub def block01(): Int32 = {
        // only a comment before the expression
        1
    }

    // record literal
    pub def record01(): { x = Int32 } = {
        // a comment before the first field must not turn the record into a block
        x = 1000
    }

    // types
    pub def type01(): (/* c */ Int32 /* c */, Int32) = (1, 2)
    pub def type02() /* c */ : /* c */ Int32 /* c */ = 1

    // local def
    pub def localDef01(): Int32 =
        // comment on a local def
        def g(x: Int32): Int32 = x;
        g(1)

}
