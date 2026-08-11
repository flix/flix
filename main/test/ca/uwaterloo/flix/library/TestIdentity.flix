mod TestIdentity {

    use Assert.{assertEq};
    use Identity.Identity;

    /////////////////////////////////////////////////////////////////////////////
    // SemiGroup                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine01(): Unit \ Assert =
        assertEq(expected = Identity(1 :: 2 :: Nil), SemiGroup.combine(Identity(1 :: Nil), Identity(2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // Monoid                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert =
        assertEq(expected = (Identity(Nil): Identity[List[Int32]]), Identity((Monoid.empty() : List[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // Functor                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert =
        assertEq(expected = Identity(1), Functor.map(x -> x + 1, Identity(0)))

    /////////////////////////////////////////////////////////////////////////////
    // Applicative                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def point01(): Unit \ Assert =
        assertEq(expected = Identity(1), (Applicative.point(1) : Identity[Int32]))

    @Test
    def ap01(): Unit \ Assert =
        assertEq(expected = Identity(2), Applicative.ap(Identity(x -> x+ 1), Identity(1)))

    /////////////////////////////////////////////////////////////////////////////
    // Monad                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert =
        assertEq(expected = Identity(2), Monad.flatMap(x -> Identity(x+1), Identity(1)))

    /////////////////////////////////////////////////////////////////////////////
    // Foldable                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert =
        assertEq(expected = 101, Foldable.foldLeft((s, x) -> s+x, 100, Identity(1)))

    @Test
    def foldRight01(): Unit \ Assert =
        assertEq(expected = 101, Foldable.foldRight((x, acc) -> acc + x, 100, Identity(1)))

    /////////////////////////////////////////////////////////////////////////////
    // Reducible                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeftTo01(): Unit \ Assert =
        // `f` is never applied for a singleton, so only `g` contributes.
        assertEq(expected = 2, Reducible.reduceLeftTo((acc, _) -> acc, x -> x + 1, Identity(1)))

    @Test
    def reduceRightTo01(): Unit \ Assert =
        assertEq(expected = 2, Reducible.reduceRightTo((_, acc) -> acc, x -> x + 1, Identity(1)))

    @Test
    def reduce01(): Unit \ Assert =
        assertEq(expected = "a", Reducible.reduce(Identity("a")))

    @Test
    def head01(): Unit \ Assert =
        assertEq(expected = 42, Reducible.head(Identity(42)))

    @Test
    def last01(): Unit \ Assert =
        assertEq(expected = 42, Reducible.last(Identity(42)))

    @Test
    def length01(): Unit \ Assert =
        assertEq(expected = 1, Reducible.length(Identity(42)))

    @Test
    def toList01(): Unit \ Assert =
        assertEq(expected = 7 :: Nil, Reducible.toList(Identity(7)))

    @Test
    def toNel01(): Unit \ Assert =
        assertEq(expected = Nel.Nel(7, Nil), Reducible.toNel(Identity(7)))

    /////////////////////////////////////////////////////////////////////////////
    // Traversable                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Unit \ Assert =
        assertEq(expected = Identity(1) :: Nil, Traversable.traverse(x -> x :: Nil, Identity(1)))

    @Test
    def sequence01(): Unit \ Assert =
        assertEq(expected = Identity(1) :: Identity(2) :: Nil, Traversable.sequence(Identity(1 :: 2 :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // Add                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def add01(): Unit \ Assert =
        assertEq(expected = Identity(3), Identity(1) + Identity(2))

    /////////////////////////////////////////////////////////////////////////////
    // Sub                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sub01(): Unit \ Assert =
        assertEq(expected = Identity(1), Identity(3) - Identity(2))

    /////////////////////////////////////////////////////////////////////////////
    // Mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mul01(): Unit \ Assert =
        assertEq(expected = Identity(6), Identity(3) * Identity(2))

    /////////////////////////////////////////////////////////////////////////////
    // Div                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def div01(): Unit \ Assert =
        assertEq(expected = Identity(3), Identity(6) / Identity(2))

    /////////////////////////////////////////////////////////////////////////////
    // Neg                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def neg01(): Unit \ Assert =
        assertEq(expected = Identity(-1), Neg.neg(Identity(1)))

    @Test
    def neg02(): Unit \ Assert =
        assertEq(expected = Identity(1), Neg.neg(Identity(-1)))

}
