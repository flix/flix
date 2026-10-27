mod Test.Comment.Exp {

    import java.lang.Runnable

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

    // infix operators
    pub def infix01(): Int32 =
        // a comment before the left operand
        1
        `plus`
        // a comment before the right operand
        2
    pub def infix02(): Int32 = 1 /* c */ + /* c */ 2
    pub def plus(x: Int32, y: Int32): Int32 = x + y

    // collection literals
    pub def collection01(): List[Int32] = List#{/* c */}
    pub def collection02(): List[Int32] = List#{/* c */ 1 /* c */, /* c */ 2 /* c */}

    // lambda or parenthesized expression
    pub def lambda04(): (Int32, Int32) -> Int32 = (x /* c */, y) -> x + y
    pub def lambda05(): Int32 -> Int32 = (x: Int32 /* c */) -> x
    pub def lambda06(): (Int32, Int32) -> Int32 = (/* c */ x, y /* c */) -> x + y
    pub def lambda07(): (Int32, Int32) -> Int32 = (x, y) /* c */ -> x + y
    pub def unit01(): Unit = (/* c */)
    pub def ascribe01(): Int32 = (1 /* c */ : /* c */ Int32)

    // record literal or block
    pub def record02(): { x = Int32, y = Int32 } =
        let r = { y = 2 };
        { /* c */ +x = 1 | r }
    pub def record03(): { x = Int32 } = { x = /* c */ 1 }
    pub def record04(): Unit =
        let _r = { /* c */ };
        ()
    pub def record05(): Unit =
        let _r = { // c
        };
        ()

    // named arguments
    pub def namedArg01(): Int32 = named(x /* c */ = 1, /* c */ y = 2)
    pub def namedArg02(): Int32 = named(x = /* c */ 1, y // c
        = 2)
    pub def named(a: { x = Int32 }, b: { y = Int32 }): Int32 = a#x + b#y

    // let with a pattern or an ascription
    pub def let02(): Int32 =
        let (x /* c */, y) = (1, 2);
        x + y
    pub def let03(): Int32 =
        let x /* c */ : /* c */ Int32 = 1;
        x

    // for fragments
    pub def forM01(): Option[Int32] =
        forM (x <- Some(1); // c
              if x > 0; /* c */ y = x + 1 // c
              ) yield y
    pub def foreach01(): Unit \ IO =
        foreach (x <- List#{1, 2}; // c
                 if /* c */ x > 1) println(x)

    // match guard
    pub def match02(x: Int32): Int32 = match x {
        case y /* c */ if /* c */ y > 0 => y
        case _ => 0
    }

    // struct literal and field access
    pub struct S[r] {
        fst: Int32
    }
    mod S {
        pub def struct01(): Int32 = region rc {
            let s = new S /* c */ @ rc { /* c */ fst = 1 };
            s/* c */->/* c */fst
        }
    }

    // if-then-else across lines
    pub def ifThenElse05(): Int32 =
        if (true) 1 // c
        else 2
    pub def ifThenElse06(): Int32 =
        if (true) { 1 } // c
        // c
        else { 2 }
    pub def ifThenElse07(): Int32 = if (false) 1 else /* c */ if (true) 2 else 3

    // run with handlers
    pub eff Ask {
        def ask(): Int32
    }
    pub eff Tell {
        def tell(x: Int32): Unit
    }
    pub def runWith01(): Int32 =
        run { // c
            Ask.ask()
        } // c
        with /* c */ handler Ask { // c
            def ask(k) = k(1) // t
            // c
        }
    pub def runWith02(): Int32 =
        run {
            Tell.tell(1); // c
            Ask.ask()
        } with handler Ask { def ask(k) = k(1) } // c
        with handler Tell { // c
            def tell(_x, k) = k() // t
        }

    // new object
    pub def newObject01(): Runnable \ IO = new Runnable { // c
        def $run(_this: Runnable): Unit = () // t
        // c
    }

    // fixpoint constraint sets
    pub def fixpoint01(): #{ A(Int32), B(Int32) } = #{ // c
        A(1). // t
        B(x) :- /* c */ A(x), // c
            A(x). // t
        // c
    }
    pub def fixpoint02(): #{ A(Int32) } = #{ // c
    }

    // local defs
    pub def localDef02(): Int32 =
        // comment on the first local def
        def g(x: Int32): Int32 = x;
        // comment on the second local def
        def h(x: Int32): Int32 = g(x) + 1;
        h(1)

}
