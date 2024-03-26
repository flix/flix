mod Test.Dec.Assoc.Type.Collection {

    ///////////////////////////////////////////////////////////////////
    // Type definitions                                              //
    ///////////////////////////////////////////////////////////////////

    pub trait Collection[c] {
        pub type Elm: Type
        pub def empty(): c
        pub def add(e: Collection.Elm[c], c: c): c
        pub def toList(c: c): List[Collection.Elm[c]]
    }

    //////////////////////////////////////////////////////////////////
    // Instances for Collection                                     //
    //////////////////////////////////////////////////////////////////

    // Instance for Option
    instance Collection[Option[a]] {
        type Elm = a
        pub def empty(): Option[a] = None
        pub def add(e: a, c: Option[a]): Option[a] = match c {
            case None    => Some(e)
            case Some(_) => c
        }
        pub def toList(c: Option[a]): List[a] = match c {
            case None    => Nil
            case Some(e) => e :: Nil
        }
    }

    // Instance for List
    instance Collection[List[a]] {
        type Elm = a
        pub def empty(): List[a] = List.empty()
        pub def add(e: a, c: List[a]): List[a] = e :: c
        pub def toList(c: List[a]): List[a] = c
    }

    // Instance for Set
    instance Collection[Set[a]] with Order[a] {
        type Elm = a
        pub def empty(): Set[a] = Set.empty()
        pub def add(e: a, c: Set[a]): Set[a] = Set.insert(e, c)
        pub def toList(c: Set[a]): List[a] = Set.toList(c)
    }

    // Instance for Vector
    instance Collection[Vector[a]] {
        type Elm = a
        pub def empty(): Vector[a] = Vector.empty()
        pub def add(e: a, c: Vector[a]): Vector[a] = Vector.append(Vector#{e}, c)
        pub def toList(c: Vector[a]): List[a] = Vector.toList(c)
    }

    // BitSet definition
    pub enum BitSet {
        case BitSet(Set[Int32])
    }

    // Instance for BitSet with undefined functions
    instance Collection[BitSet] {
        type Elm = Int32
        pub def empty(): BitSet = ???
        pub def add(e: Int32, c: BitSet): BitSet = ???
        pub def toList(c: BitSet): List[Int32] = ???
    }


    ///////////////////////////////////////////////////////////////////
    // Polymorphic functions that use Collection                     //
    ///////////////////////////////////////////////////////////////////

    pub def addTwiceThenToList(e1: Collection.Elm[c], e2: Collection.Elm[c], c: c): List[Collection.Elm[c]] with Collection[c] =
        Collection.toList(Collection.add(e2, Collection.add(e1, c)))

    pub def toListThenAddTwice(e1: Collection.Elm[c], e2: Collection.Elm[c], c: c): List[Collection.Elm[c]] with Collection[c] =
        Collection.add(e2, Collection.add(e1, Collection.toList(c)))


    ///////////////////////////////////////////////////////////////////
    // Monomorphic functions that use Collection                     //
    ///////////////////////////////////////////////////////////////////

    // Option

    pub def addTwiceThenToListOption01(e1: Int32, e2: Int32, c: Option[Int32]): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, c)))

    pub def toListThenAddTwiceOption01(e1: Int32, e2: Int32, c: Option[Int32]): List[Int32] =
        Collection.add(e2, Collection.add(e1, Collection.toList(c)))

    pub def addTwiceThenToListOption02(e1: Int32, e2: Int32, c: Option[Int32]): List[Int32] =
        addTwiceThenToList(e1, e2, c)

    pub def toListThenAddTwiceOption02(e1: Int32, e2: Int32, c: Option[Int32]): List[Int32] =
        toListThenAddTwice(e1, e2, c)


    // List

    pub def addTwiceThenToListList01(e1: Int32, e2: Int32, c: List[Int32]): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, c)))

    pub def toListThenAddTwiceList01(e1: Int32, e2: Int32, c: List[Int32]): List[Int32] =
        Collection.add(e2, Collection.add(e1, Collection.toList(c)))

    pub def addTwiceThenToListList02(e1: Int32, e2: Int32, c: List[Int32]): List[Int32] =
        addTwiceThenToList(e1, e2, c)

    pub def toListThenAddTwiceList02(e1: Int32, e2: Int32, c: List[Int32]): List[Int32] =
        toListThenAddTwice(e1, e2, c)


    // Set

    pub def addTwiceThenToListSet01(e1: Int32, e2: Int32, c: Set[Int32]): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, c)))

    pub def toListThenAddTwiceSet01(e1: Int32, e2: Int32, c: Set[Int32]): List[Int32] =
        Collection.add(e2, Collection.add(e1, Collection.toList(c)))

    pub def addTwiceThenToListSet02(e1: Int32, e2: Int32, c: Set[Int32]): List[Int32] =
        addTwiceThenToList(e1, e2, c)

    pub def toListThenAddTwiceSet02(e1: Int32, e2: Int32, c: Set[Int32]): List[Int32] =
        toListThenAddTwice(e1, e2, c)


    // Vector

    pub def addTwiceThenToListVector01(e1: Int32, e2: Int32, c: Vector[Int32]): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, c)))

    pub def toListThenAddTwiceVector01(e1: Int32, e2: Int32, c: Vector[Int32]): List[Int32] =
        Collection.add(e2, Collection.add(e1, Collection.toList(c)))

    pub def addTwiceThenToListVector02(e1: Int32, e2: Int32, c: Vector[Int32]): List[Int32] =
        addTwiceThenToList(e1, e2, c)

    pub def toListThenAddTwiceVector02(e1: Int32, e2: Int32, c: Vector[Int32]): List[Int32] =
        toListThenAddTwice(e1, e2, c)


    // BitSet

    pub def addTwiceThenToListBitSet01(e1: Int32, e2: Int32, c: BitSet): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, c)))

    pub def toListThenAddTwiceBitSet01(e1: Int32, e2: Int32, c: BitSet): List[Int32] =
        Collection.add(e2, Collection.add(e1, Collection.toList(c)))

    pub def addTwiceThenToListBitSet02(e1: Int32, e2: Int32, c: BitSet): List[Int32] =
        addTwiceThenToList(e1, e2, c)

    pub def toListThenAddTwiceBitSet02(e1: Int32, e2: Int32, c: BitSet): List[Int32] =
        toListThenAddTwice(e1, e2, c)


    ///////////////////////////////////////////////////////////////////
    // Tests for Option                                              //
    ///////////////////////////////////////////////////////////////////

    @Test
    def testOption01(): Bool = {
        let c: Option[Int32] = Collection.empty();
        None == c
    }

    @Test
    def testOption02(): Bool = {
        let c = Collection.add(1, Collection.empty());
        Some(1) == c
    }

    @Test
    def testOption03(): Bool = {
        let c = Collection.add(2, Collection.add(1, Collection.empty()));
        Some(1) == c
    }

    @Test
    def testOption04(): Bool = {
        let l = addTwiceThenToList(1, 2, None);
        List#{1} == l
    }

    @Test
    def testOption05(): Bool = {
        let l = toListThenAddTwice(2, 1, None);
        List#{1, 2} == l
    }

    @Test
    def testOption06(): Bool = {
        let l = addTwiceThenToListOption01(1, 2, None);
        List#{1} == l
    }

    @Test
    def testOption07(): Bool = {
        let l = addTwiceThenToListOption02(1, 2, None);
        List#{1} == l
    }

    @Test
    def testOption08(): Bool = {
        let l = toListThenAddTwiceOption01(1, 2, None);
        List#{2, 1} == l
    }

    @Test
    def testOption09(): Bool = {
        let l = toListThenAddTwiceOption02(1, 2, None);
        List#{2, 1} == l
    }


    ///////////////////////////////////////////////////////////////////
    // Tests for List                                                //
    ///////////////////////////////////////////////////////////////////

    @Test
    def testList01(): Bool = {
        let c: List[Int32] = Collection.empty();
        Nil == c
    }

    @Test
    def testList02(): Bool = {
        let c = Collection.add(1, Collection.empty());
        List#{1} == c
    }

    @Test
    def testList03(): Bool = {
        let c = Collection.add(2, Collection.add(1, Collection.empty()));
        List#{2, 1} == c
    }

    @Test
    def testList04(): Bool = {
        let l = addTwiceThenToList(1, 2, Nil);
        List#{2, 1} == l
    }

    @Test
    def testList05(): Bool = {
        let l = toListThenAddTwice(2, 1, Nil);
        List#{1, 2} == l
    }

    @Test
    def testList06(): Bool = {
        let l = addTwiceThenToListList01(1, 2, Nil);
        List#{2, 1} == l
    }

    @Test
    def testList07(): Bool = {
        let l = addTwiceThenToListList02(1, 2, Nil);
        List#{2, 1} == l
    }

    @Test
    def testList08(): Bool = {
        let l = toListThenAddTwiceList01(0, 1, Nil);
        List#{1, 0} == l
    }

    @Test
    def testList09(): Bool = {
        let l = toListThenAddTwiceList02(3, 4, Nil);
        List#{4, 3} == l
    }


    ///////////////////////////////////////////////////////////////////
    // Tests for Set                                                 //
    ///////////////////////////////////////////////////////////////////

    @Test
    def testSet01(): Bool = {
        let c: Set[Int32] = Collection.empty();
        Set#{} == c
    }

    @Test
    def testSet02(): Bool = {
        let c = Collection.add(1, Collection.empty());
        Set#{1} == c
    }

    @Test
    def testSet03(): Bool = {
        let c = Collection.add(2, Collection.add(1, Collection.empty()));
        Set#{2, 1} == c
    }

    @Test
    def testSet04(): Bool = {
        let l = addTwiceThenToList(1, 2, Set#{});
        List#{1, 2} == l
    }

    @Test
    def testSet05(): Bool = {
        let l = toListThenAddTwice(2, 1, Set#{});
        List#{1, 2} == l
    }

    @Test
    def testSet06(): Bool = {
        let l = addTwiceThenToListSet01(1, 2, Set#{});
        List#{1, 2} == l
    }

    @Test
    def testSet07(): Bool = {
        let l = toListThenAddTwiceSet02(100, 200, Set#{});
        List#{200, 100} == l
    }

    @Test
    def testSet08(): Bool = {
        let l = toListThenAddTwiceSet01(0, 1, Set#{});
        List#{1, 0} == l
    }

    @Test
    def testSet09(): Bool = {
        let l = toListThenAddTwiceSet02(3, 4, Set#{});
        List#{4, 3} == l
    }

    @Test
    def testSet10(): Bool = {
        let l = toListThenAddTwiceSet01(5, 5, Set#{});
        List#{5, 5} == l
    }

    @Test
    def testSet11(): Bool = {
        let l = addTwiceThenToListSet01(5, 5, Set#{});
        List#{5} == l
    }


    ///////////////////////////////////////////////////////////////////
    // Tests for Vector                                              //
    ///////////////////////////////////////////////////////////////////

    @Test
    def testVector01(): Bool = {
        let c: Vector[Int32] = Collection.empty();
        Vector#{} == c
    }

    @Test
    def testVector02(): Bool = {
        let c = Collection.add(1, Collection.empty());
        Vector#{1} == c
    }

    @Test
    def testVector03(): Bool = {
        let c = Collection.add(2, Collection.add(1, Collection.empty()));
        Vector#{2, 1} == c
    }

    @Test
    def testVector04(): Bool = {
        let l = addTwiceThenToList(1, 2, Vector#{});
        List#{2, 1} == l
    }

    @Test
    def testVector05(): Bool = {
        let l = toListThenAddTwice(2, 1, Vector#{});
        List#{1, 2} == l
    }

    @Test
    def testVector06(): Bool = {
        let l = addTwiceThenToListVector01(1, 2, Vector#{});
        List#{2, 1} == l
    }

    @Test
    def testVector07(): Bool = {
        let l = toListThenAddTwiceVector02(100, 200, Vector#{});
        List#{200, 100} == l
    }

    @Test
    def testVector08(): Bool = {
        let l = toListThenAddTwiceVector01(0, 1, Vector#{});
        List#{1, 0} == l
    }

    @Test
    def testVector09(): Bool = {
        let l = toListThenAddTwiceVector02(3, 4, Vector#{});
        List#{4, 3} == l
    }

    @Test
    def testVector10(): Bool = {
        let l = toListThenAddTwiceVector01(5, 5, Vector#{});
        List#{5, 5} == l
    }

    @Test
    def testVector11(): Bool = {
        let l = addTwiceThenToListVector01(5, 5, Vector#{});
        List#{5, 5} == l
    }
}
