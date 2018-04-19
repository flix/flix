/////////////////////////////////////////////////////////////////////////////
// Spawn expressions                                                       //
/////////////////////////////////////////////////////////////////////////////

enum Color {
    case Red,
    case Blue,
    case New(Str)
}

def f01(): Unit = ()

def testSpawn01(): Unit = spawn f01()

def f02(x: Int): Unit = ()

def testSpawn02(): Unit = spawn f02(42)

def f03(x: Int, y: Int): Unit = ()

def testSpawn03(): Unit = spawn f03(42, 10)

def f04(x: Str): Unit = ()

def testSpawn04(): Unit = spawn f04("str")

def f05(x: Str, y: Str): Unit = ()

def testSpawn05(): Unit = spawn f05("str", "strstr")

//def f06(x: Float32): Unit = ()
//
//def testSpawn06(): Unit = spawn f06(1.2f)
//
//def f07(x: Float32, y: Float32): Unit = ()
//
//def testSpawn07(): Unit = spawn f07()

def f08(x: Bool): Unit = ()

def testSpawn08(): Unit = spawn f08(true)

def f09(x: Bool, y: Bool): Unit = ()

def testSpawn09(): Unit = spawn f09(true, false)

def f10(x: Int -> Int): Unit = ()

def testSpawn10(): Unit = spawn f10(x -> x)

def f11(x: Int -> Int, y: Int -> Int): Unit = ()

def testSpawn11(): Unit = spawn f11(x -> x, x -> x + 2)

def f12(x: (Int, Int)): Unit = ()

def testSpawn12(): Unit = spawn f12((2, 3))

def f13(x: (Int, Int), y: (Int, Int)): Unit = ()

def testSpawn13(): Unit = spawn f13((2, 3), (3, 4))

def f14(x: Channel[Int]): Unit = ()

def testSpawn14(): Unit = spawn f14(channel Int)

def f16(): Int = 42

def testSpawn16(): Unit = spawn f16()

def f17(): Str = "str"

def testSpawn17(): Unit = spawn f17()

def f18(): (Int, Int) = (10, 42)

def testSpawn18(): Unit = spawn f18()

def f19(): Color = Red

def testSpawn19(): Unit = spawn f19()

def f20(): Color = Color.New("Green")

def testSpawn20(): Unit = spawn f20()
