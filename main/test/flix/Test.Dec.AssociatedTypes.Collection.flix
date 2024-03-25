mod Test.Dec.AssociatedTypes.Collection {

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
        Collection.toList(Collection.add(e2, Collection.add(e1, Collection.toList(c))))


    ///////////////////////////////////////////////////////////////////
    // Monomorphic functions that use Collection                     //
    ///////////////////////////////////////////////////////////////////

    // Option

    pub def addTwiceThenToListOption01(e1: Int32, e2: Int32, c: Option[Int32]): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, c)))

    pub def toListThenAddTwiceOption01(e1: Int32, e2: Int32, c: Option[Int32]): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, Collection.toList(c))))

    pub def addTwiceThenToListOption02(e1: Int32, e2: Int32, c: Option[Int32]): List[Int32] =
        addTwiceThenToList(e1, e2, c)

    pub def toListThenAddTwiceOption02(e1: Int32, e2: Int32, c: Option[Int32]): List[Int32] =
        toListThenAddTwice(e1, e2, c)


    // List

    pub def addTwiceThenToListList01(e1: Int32, e2: Int32, c: List[Int32]): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, c)))

    pub def toListThenAddTwiceList01(e1: Int32, e2: Int32, c: List[Int32]): List[Int32] =
        Collection.toList(Collection.add(e2, Collection.add(e1, Collection.toList(c))))

    pub def addTwiceThenToListList02(e1: Int32, e2: Int32, c: List[Int32]): List[Int32] =
        addTwiceThenToList(e1, e2, c)

    pub def toListThenAddTwiceList02(e1: Int32, e2: Int32, c: List[Int32]): List[Int32] =
        toListThenAddTwice(e1, e2, c)


    ///////////////////////////////////////////////////////////////////
    // Tests                                                         //
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
}
