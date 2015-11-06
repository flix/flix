namespace NativeTest {

    // sample lattice defined over Flix Str type, using native values/methods
    let Str<> = (
        #ca.uwaterloo.flix.util.misc.SampleLattice.BOT,
        #ca.uwaterloo.flix.util.misc.SampleLattice.TOP,
        #ca.uwaterloo.flix.util.misc.SampleLattice.leq,
        #ca.uwaterloo.flix.util.misc.SampleLattice.lub,
        #ca.uwaterloo.flix.util.misc.SampleLattice.glb
    );

    // not1 to not4 are native methods that take (and return) primitive or boxed booleans.
    // not5 takes and returns Flix booleans.
    // in testBool, we pass every combination of Flix, Java primitive, and Java boxed booleans.
    def not5(b: Bool): Bool = !b;
    def testBool(i: Int): Bool = match i with {
        case 0  => #ca.uwaterloo.flix.util.misc.JavaNative.not1(true);
        case 1  => #ca.uwaterloo.flix.util.misc.JavaNative.not1(#java.lang.Boolean.TRUE);
        case 2  => #ca.uwaterloo.flix.util.misc.JavaNative.not1(#ca.uwaterloo.flix.util.misc.JavaNative.TRUE);
        case 3  => #ca.uwaterloo.flix.util.misc.JavaNative.not1(false);
        case 4  => #ca.uwaterloo.flix.util.misc.JavaNative.not1(#java.lang.Boolean.FALSE);
        case 5  => #ca.uwaterloo.flix.util.misc.JavaNative.not1(#ca.uwaterloo.flix.util.misc.JavaNative.FALSE);

        case 6  => #ca.uwaterloo.flix.util.misc.JavaNative.not2(true);
        case 7  => #ca.uwaterloo.flix.util.misc.JavaNative.not2(#java.lang.Boolean.TRUE);
        case 8  => #ca.uwaterloo.flix.util.misc.JavaNative.not2(#ca.uwaterloo.flix.util.misc.JavaNative.TRUE);
        case 9  => #ca.uwaterloo.flix.util.misc.JavaNative.not2(false);
        case 10 => #ca.uwaterloo.flix.util.misc.JavaNative.not2(#java.lang.Boolean.FALSE);
        case 11 => #ca.uwaterloo.flix.util.misc.JavaNative.not2(#ca.uwaterloo.flix.util.misc.JavaNative.FALSE);

        case 12 => #ca.uwaterloo.flix.util.misc.JavaNative.not3(true);
        case 13 => #ca.uwaterloo.flix.util.misc.JavaNative.not3(#java.lang.Boolean.TRUE);
        case 14 => #ca.uwaterloo.flix.util.misc.JavaNative.not3(#ca.uwaterloo.flix.util.misc.JavaNative.TRUE);
        case 15 => #ca.uwaterloo.flix.util.misc.JavaNative.not3(false);
        case 16 => #ca.uwaterloo.flix.util.misc.JavaNative.not3(#java.lang.Boolean.FALSE);
        case 17 => #ca.uwaterloo.flix.util.misc.JavaNative.not3(#ca.uwaterloo.flix.util.misc.JavaNative.FALSE);

        case 18  => #ca.uwaterloo.flix.util.misc.JavaNative.not4(true);
        case 19 => #ca.uwaterloo.flix.util.misc.JavaNative.not4(#java.lang.Boolean.TRUE);
        case 20 => #ca.uwaterloo.flix.util.misc.JavaNative.not4(#ca.uwaterloo.flix.util.misc.JavaNative.TRUE);
        case 21 => #ca.uwaterloo.flix.util.misc.JavaNative.not4(false);
        case 22 => #ca.uwaterloo.flix.util.misc.JavaNative.not4(#java.lang.Boolean.FALSE);
        case 23 => #ca.uwaterloo.flix.util.misc.JavaNative.not4(#ca.uwaterloo.flix.util.misc.JavaNative.FALSE);

        case 24 => not5(true);
        case 25 => not5(#java.lang.Boolean.TRUE);
        case 26 => not5(#ca.uwaterloo.flix.util.misc.JavaNative.TRUE);
        case 27 => not5(false);
        case 28 => not5(#java.lang.Boolean.FALSE);
        case 29 => not5(#ca.uwaterloo.flix.util.misc.JavaNative.FALSE);
    };

    // increment1 to increment4 are native methods that take (and return) primitive or boxed ints.
    // increment5 takes and returns Flix ints.
    // in testInt, we pass every combination of Flix, Java primitive, and Java boxed ints.
    def increment5(i: Int): Int = i + 1;
    def testInt(i: Int): Int = match i with {
        case 0  => #ca.uwaterloo.flix.util.misc.JavaNative.increment1(1);
        case 1  => #ca.uwaterloo.flix.util.misc.JavaNative.increment1(#ca.uwaterloo.flix.util.misc.JavaNative.primitiveInt);
        case 2  => #ca.uwaterloo.flix.util.misc.JavaNative.increment1(#ca.uwaterloo.flix.util.misc.JavaNative.boxedInt);

        case 3  => #ca.uwaterloo.flix.util.misc.JavaNative.increment2(1);
        case 4  => #ca.uwaterloo.flix.util.misc.JavaNative.increment2(#ca.uwaterloo.flix.util.misc.JavaNative.primitiveInt);
        case 5  => #ca.uwaterloo.flix.util.misc.JavaNative.increment2(#ca.uwaterloo.flix.util.misc.JavaNative.boxedInt);

        case 6  => #ca.uwaterloo.flix.util.misc.JavaNative.increment3(1);
        case 7  => #ca.uwaterloo.flix.util.misc.JavaNative.increment3(#ca.uwaterloo.flix.util.misc.JavaNative.primitiveInt);
        case 8  => #ca.uwaterloo.flix.util.misc.JavaNative.increment3(#ca.uwaterloo.flix.util.misc.JavaNative.boxedInt);

        case 9  => #ca.uwaterloo.flix.util.misc.JavaNative.increment4(1);
        case 10  => #ca.uwaterloo.flix.util.misc.JavaNative.increment4(#ca.uwaterloo.flix.util.misc.JavaNative.primitiveInt);
        case 11 => #ca.uwaterloo.flix.util.misc.JavaNative.increment4(#ca.uwaterloo.flix.util.misc.JavaNative.boxedInt);

        case 12 => increment5(1);
        case 13 => increment5(#ca.uwaterloo.flix.util.misc.JavaNative.primitiveInt);
        case 14 => increment5(#ca.uwaterloo.flix.util.misc.JavaNative.boxedInt);
    }

    // streq compares two strings for equality.
    // in testStreq, we pass combinations of Flix and Java strings.
    def testStreq(i: Int): Bool = match i with {
        case 0  => #ca.uwaterloo.flix.util.misc.JavaNative.streq("hello", "hello");
        case 1  => #ca.uwaterloo.flix.util.misc.JavaNative.streq("hello", "world");
        case 2  => #ca.uwaterloo.flix.util.misc.JavaNative.streq("world", "hello");
        case 3  => #ca.uwaterloo.flix.util.misc.JavaNative.streq("world", "world");

        case 4  => #ca.uwaterloo.flix.util.misc.JavaNative.streq("hello", #ca.uwaterloo.flix.util.misc.JavaNative.hello);
        case 5  => #ca.uwaterloo.flix.util.misc.JavaNative.streq("hello", #ca.uwaterloo.flix.util.misc.JavaNative.world);
        case 6  => #ca.uwaterloo.flix.util.misc.JavaNative.streq("world", #ca.uwaterloo.flix.util.misc.JavaNative.hello);
        case 7  => #ca.uwaterloo.flix.util.misc.JavaNative.streq("world", #ca.uwaterloo.flix.util.misc.JavaNative.world);

        case 8  => #ca.uwaterloo.flix.util.misc.JavaNative.streq(#ca.uwaterloo.flix.util.misc.JavaNative.hello, "hello");
        case 9  => #ca.uwaterloo.flix.util.misc.JavaNative.streq(#ca.uwaterloo.flix.util.misc.JavaNative.hello, "world");
        case 10 => #ca.uwaterloo.flix.util.misc.JavaNative.streq(#ca.uwaterloo.flix.util.misc.JavaNative.world, "hello");
        case 11 => #ca.uwaterloo.flix.util.misc.JavaNative.streq(#ca.uwaterloo.flix.util.misc.JavaNative.world, "world");

        case 12 => #ca.uwaterloo.flix.util.misc.JavaNative.streq(#ca.uwaterloo.flix.util.misc.JavaNative.hello, #ca.uwaterloo.flix.util.misc.JavaNative.hello);
        case 13 => #ca.uwaterloo.flix.util.misc.JavaNative.streq(#ca.uwaterloo.flix.util.misc.JavaNative.hello, #ca.uwaterloo.flix.util.misc.JavaNative.world);
        case 14 => #ca.uwaterloo.flix.util.misc.JavaNative.streq(#ca.uwaterloo.flix.util.misc.JavaNative.world, #ca.uwaterloo.flix.util.misc.JavaNative.hello);
        case 15 => #ca.uwaterloo.flix.util.misc.JavaNative.streq(#ca.uwaterloo.flix.util.misc.JavaNative.world, #ca.uwaterloo.flix.util.misc.JavaNative.world);
    }

    // strcat concatenates two strings.
    // in testStrcat, we pass combinations of Flix and Java strings.
    def testStrcat(i: Int): Str = match i with {
        case 0  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat("hello", "hello");
        case 1  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat("hello", "world");
        case 2  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat("world", "hello");
        case 3  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat("world", "world");

        case 4  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat("hello", #ca.uwaterloo.flix.util.misc.JavaNative.hello);
        case 5  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat("hello", #ca.uwaterloo.flix.util.misc.JavaNative.world);
        case 6  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat("world", #ca.uwaterloo.flix.util.misc.JavaNative.hello);
        case 7  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat("world", #ca.uwaterloo.flix.util.misc.JavaNative.world);

        case 8  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat(#ca.uwaterloo.flix.util.misc.JavaNative.hello, "hello");
        case 9  => #ca.uwaterloo.flix.util.misc.JavaNative.strcat(#ca.uwaterloo.flix.util.misc.JavaNative.hello, "world");
        case 10 => #ca.uwaterloo.flix.util.misc.JavaNative.strcat(#ca.uwaterloo.flix.util.misc.JavaNative.world, "hello");
        case 11 => #ca.uwaterloo.flix.util.misc.JavaNative.strcat(#ca.uwaterloo.flix.util.misc.JavaNative.world, "world");

        case 12 => #ca.uwaterloo.flix.util.misc.JavaNative.strcat(#ca.uwaterloo.flix.util.misc.JavaNative.hello, #ca.uwaterloo.flix.util.misc.JavaNative.hello);
        case 13 => #ca.uwaterloo.flix.util.misc.JavaNative.strcat(#ca.uwaterloo.flix.util.misc.JavaNative.hello, #ca.uwaterloo.flix.util.misc.JavaNative.world);
        case 14 => #ca.uwaterloo.flix.util.misc.JavaNative.strcat(#ca.uwaterloo.flix.util.misc.JavaNative.world, #ca.uwaterloo.flix.util.misc.JavaNative.hello);
        case 15 => #ca.uwaterloo.flix.util.misc.JavaNative.strcat(#ca.uwaterloo.flix.util.misc.JavaNative.world, #ca.uwaterloo.flix.util.misc.JavaNative.world);
    }

    // Fields in a Scala object are compiled as static methods
    def strTuple2(i: Int): (#java.lang.Object, #java.lang.Object) = #ca.uwaterloo.flix.util.misc.ScalaNative.strTuple2()
    def tuple6(i: Int): #scala.Tuple6 = #ca.uwaterloo.flix.util.misc.ScalaNative.tuple6()

    def mkTuple2(t1: Int, t2: Int): (#java.lang.Object, #java.lang.Object) =
        #ca.uwaterloo.flix.util.misc.ScalaNative.mkTuple2(t1, t2)
    def incrTuple2(tpl: (#java.lang.Object, #java.lang.Object)): (#java.lang.Object, #java.lang.Object) =
        #ca.uwaterloo.flix.util.misc.ScalaNative.incrTuple2(tpl)
    def mkTuple3(t1: Int, t2: Int, t3: Int): (#java.lang.Object, #java.lang.Object, #java.lang.Object) =
        #ca.uwaterloo.flix.util.misc.ScalaNative.mkTuple3(t1, t2, t3)
    def incrTuple3(tpl: (#java.lang.Object, #java.lang.Object, #java.lang.Object)): (#java.lang.Object, #java.lang.Object, #java.lang.Object) =
        #ca.uwaterloo.flix.util.misc.ScalaNative.incrTuple3(tpl)
    def mkTuple4(t1: Int, t2: Int, t3: Int, t4: Int): (#java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object) =
        #ca.uwaterloo.flix.util.misc.ScalaNative.mkTuple4(t1, t2, t3, t4)
    def incrTuple4(tpl: (#java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object)): (#java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object) =
        #ca.uwaterloo.flix.util.misc.ScalaNative.incrTuple4(tpl)
    def mkTuple5(t1: Int, t2: Int, t3: Int, t4: Int, t5: Int): (#java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object) =
        #ca.uwaterloo.flix.util.misc.ScalaNative.mkTuple5(t1, t2, t3, t4, t5)
    def incrTuple5(tpl: (#java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object)): (#java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object) =
        #ca.uwaterloo.flix.util.misc.ScalaNative.incrTuple5(tpl)

    def reverseTuple6(t: #scala.Tuple6): #scala.Tuple6 =
        #ca.uwaterloo.flix.util.misc.ScalaNative.reverseTuple6(t)

    def set(i: Int): Set[Int] = (#ca.uwaterloo.flix.util.misc.ScalaNative.set: (Int) -> Set[Int])()
    def mkSet(i: Int): Set[Int] = (#ca.uwaterloo.flix.util.misc.ScalaNative.mkSet: (Int) -> Set[Int])(i)
    def incrSetBy(s: Set[Int], i: Int): Set[Int] =
        (#ca.uwaterloo.flix.util.misc.ScalaNative.incrSetBy: (Set[Int], Int) -> Set[Int])(s, i)

    /* Expected result
       +---+-------+-------+-------+-------+-------+
       | i | b1    | b2    | b3    | b4    | b5    |
       +---+-------+-------+-------+-------+-------+
       | 0 | false | false | false | false | false |
       | 1 | false | false | false | false | false |
       | 2 | false | false | false | false | false |
       | 3 | true  | true  | true  | true  | true  |
       | 4 | true  | true  | true  | true  | true  |
       | 5 | true  | true  | true  | true  | true  |
       +---+-------+-------+-------+-------+-------+
    */
    rel Not(i: Int, b1: Bool, b2: Bool, b3: Bool, b4: Bool, b5: Bool);
    Not(0, testBool(0), testBool(6), testBool(12), testBool(18), testBool(24)).
    Not(1, testBool(1), testBool(7), testBool(13), testBool(19), testBool(25)).
    Not(2, testBool(2), testBool(8), testBool(14), testBool(20), testBool(26)).
    Not(3, testBool(3), testBool(9), testBool(15), testBool(21), testBool(27)).
    Not(4, testBool(4), testBool(10), testBool(16), testBool(22), testBool(28)).
    Not(5, testBool(5), testBool(11), testBool(17), testBool(23), testBool(29)).
    print Not.

    /* Expected result
       +---+----+----+----+----+----+
       | i | i1 | i2 | i3 | i4 | i5 |
       +---+----+----+----+----+----+
       | 0 | 2  | 2  | 2  | 2  | 2  |
       | 1 | 2  | 2  | 2  | 2  | 2  |
       | 2 | 2  | 2  | 2  | 2  | 2  |
       +---+----+----+----+----+----+
    */
    rel Increment(i: Int, i1: Int, i2: Int, i3: Int, i4: Int, i5: Int);
    Increment(0, testInt(0), testInt(3), testInt(6), testInt(9), testInt(12)).
    Increment(1, testInt(1), testInt(4), testInt(7), testInt(10), testInt(13)).
    Increment(2, testInt(2), testInt(5), testInt(8), testInt(11), testInt(14)).
    print Increment.

    /* Expected result
       +---+-------+-------+-------+-------+
       | i | b1    | b2    | b3    | b4    |
       +---+-------+-------+-------+-------+
       | 0 | true  | true  | true  | true  |
       | 1 | false | false | false | false |
       | 2 | false | false | false | false |
       | 3 | true  | true  | true  | true  |
       +---+-------+-------+-------+-------+
    */
    rel Streq(i: Int, b1: Bool, b2: Bool, b3: Bool, b4: Bool);
    Streq(0, testStreq(0), testStreq(4), testStreq(8), testStreq(12)).
    Streq(1, testStreq(1), testStreq(5), testStreq(9), testStreq(13)).
    Streq(2, testStreq(2), testStreq(6), testStreq(10), testStreq(14)).
    Streq(3, testStreq(3), testStreq(7), testStreq(11), testStreq(15)).
    print Streq.

    /* Expected result
       +---+------------+------------+------------+------------+
       | i | b1         | b2         | b3         | b4         |
       +---+------------+------------+------------+------------+
       | 0 | hellohello | hellohello | hellohello | hellohello |
       | 1 | helloworld | helloworld | helloworld | helloworld |
       | 2 | worldhello | worldhello | worldhello | worldhello |
       | 3 | worldworld | worldworld | worldworld | worldworld |
       +---+------------+------------+------------+------------+
    */
    rel Strcat(i: Int, b1: Str, b2: Str, b3: Str, b4: Str);
    Strcat(0, testStrcat(0), testStrcat(4), testStrcat(8), testStrcat(12)).
    Strcat(1, testStrcat(1), testStrcat(5), testStrcat(9), testStrcat(13)).
    Strcat(2, testStrcat(2), testStrcat(6), testStrcat(10), testStrcat(14)).
    Strcat(3, testStrcat(3), testStrcat(7), testStrcat(11), testStrcat(15)).
    print Strcat.

    /* Expected result
       +------+-------+------+-------+------+-------+
       | b1   | b2    | b3   | b4    | b5   | b6    |
       +------+-------+------+-------+------+-------+
       | true | false | true | false | true | false |
       +------+-------+------+-------+------+-------+
    */
    rel BoolVals(b1: Bool, b2: Bool, b3: Bool, b4: Bool, b5: Bool, b6: Bool);
    BoolVals(#java.lang.Boolean.TRUE,
             #java.lang.Boolean.FALSE,
             #ca.uwaterloo.flix.util.misc.JavaNative.TRUE,
             #ca.uwaterloo.flix.util.misc.JavaNative.FALSE,
             true,
             false).
    print BoolVals.

    /* Expected result
       +----+----+----+
       | i1 | i2 | i3 |
       +----+----+----+
       | 1  | 1  | 1  |
       +----+----+----+
    */
    rel IntVals(i1: Int, i2: Int, i3: Int);
    IntVals(#ca.uwaterloo.flix.util.misc.JavaNative.primitiveInt,
            #ca.uwaterloo.flix.util.misc.JavaNative.boxedInt,
            1).
    print IntVals.

    /* Expected result
       +-------+-------+
       | s1    | s2    |
       +-------+-------+
       | hello | world |
       +-------+-------+
    */
    rel StrVals(s1: Str, s2: Str);
    StrVals(#ca.uwaterloo.flix.util.misc.JavaNative.hello, "world").
    print StrVals.

    /* Expected result
       +---------------------------+
       | t                         |
       +---------------------------+
       | (Native(101),Native(201)) |
       | (Native(5),Native(6))     |
       | (Native(foo),Native(bar)) |
       +---------------------------+
    */
    rel Tuple2(t: (#java.lang.Object, #java.lang.Object));
    Tuple2(strTuple2(1)).
    Tuple2(mkTuple2(5, 6)).
    Tuple2(incrTuple2(mkTuple2(100, 200))).
    print Tuple2.

    /* Expected result
       +---------------------------------+
       | t                               |
       +---------------------------------+
       | (Native(1),Native(2),Native(3)) |
       | (Native(5),Native(6),Native(7)) |
       +---------------------------------+
    */
    rel Tuple3(t: (#java.lang.Object, #java.lang.Object, #java.lang.Object));
    Tuple3(mkTuple3(1, 2, 3)).
    Tuple3(incrTuple3(mkTuple3(4, 5, 6))).
    print Tuple3.

    /* Expected result
       +------------------------------------------------+
       | t                                              |
       +------------------------------------------------+
       | (Native(100),Native(99),Native(98),Native(97)) |
       | (Native(99),Native(98),Native(97),Native(96))  |
       +------------------------------------------------+
    */
    rel Tuple4(t: (#java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object));
    Tuple4(mkTuple4(99, 98, 97, 96)).
    Tuple4(incrTuple4(mkTuple4(99, 98, 97, 96))).
    print Tuple4.

    /* Expected result
       +-----------------------------------------------------+
       | t                                                   |
       +-----------------------------------------------------+
       | (Native(0),Native(0),Native(0),Native(0),Native(0)) |
       | (Native(1),Native(1),Native(1),Native(1),Native(1)) |
       +-----------------------------------------------------+
    */
    rel Tuple5(t: (#java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object, #java.lang.Object));
    Tuple5(mkTuple5(0, 0, 0, 0, 0)).
    Tuple5(incrTuple5(mkTuple5(0, 0, 0, 0, 0))).
    print Tuple5.

    /* Expected result
       +---------------------------------------+
       | t                                     |
       +---------------------------------------+
       | Native((42,(foo,bar),5,1,false,true)) |
       | Native((true,false,1,5,(foo,bar),42)) |
       +---------------------------------------+
    */
    rel Tuple6(t: #scala.Tuple6);
    Tuple6(tuple6(0)).
    Tuple6(reverseTuple6(tuple6(0))).
    print Tuple6.

    /* Expected result (order may be different)
       +---+-----------------------------+
       | i | s                           |
       +---+-----------------------------+
       | 0 | Native(Set(1, 2, 3))        |
       | 1 | Native(Set(2, 3, 4))        |
       | 2 | Native(Set(3, 4, 5))        |
       | 3 | Native(Set(0, 1, 2, 3))     |
       | 4 | Native(Set(10, 11, 12, 13)) |
       | 5 | Native(Set(5, 6, 7, 3, 4))  |
       +---+-----------------------------+
    */
    rel Set(i: Int, s: Set[Int]);
    Set(0, set(1)).
    Set(1, incrSetBy(set(1), 1)).
    Set(2, incrSetBy(set(1), 2)).
    Set(3, mkSet(4)).
    Set(4, incrSetBy(mkSet(4), 10)).
    Set(5, incrSetBy(mkSet(5), 3)).
    print Set.
}