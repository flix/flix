/////////////////////////////////////////////////////////////////////////////
// Spawn expressions                                                       //
/////////////////////////////////////////////////////////////////////////////

enum Color {
    case Red,
    case Blue,
    case New(Str)
}

def f01(): Unit = ()

@test
def testSpawn01(): Unit = spawn f01()

def f02(x: Int): Unit = ()

@test
def testSpawn02(): Unit = spawn f02(42)

def f03(x: Int, y: Int): Unit = ()

@test
def testSpawn03(): Unit = spawn f03(42, 10)

def f04(x: Str): Unit = ()

@test
def testSpawn04(): Unit = spawn f04("str")

def f05(x: Str, y: Str): Unit = ()

@test
def testSpawn05(): Unit = spawn f05("str", "strstr")

//def f06(x: Float32): Unit = ()
//
//def testSpawn06(): Unit = spawn f06(1.2f)
//
//def f07(x: Float32, y: Float32): Unit = ()
//
//def testSpawn07(): Unit = spawn f07()

def f08(x: Bool): Unit = ()

@test
def testSpawn08(): Unit = spawn f08(true)

def f09(x: Bool, y: Bool): Unit = ()

@test
def testSpawn09(): Unit = spawn f09(true, false)

def f10(x: Int -> Int): Unit = ()

@test
def testSpawn10(): Unit = spawn f10(x -> x)

def f11(x: Int -> Int, y: Int -> Int): Unit = ()

@test
def testSpawn11(): Unit = spawn f11(x -> x, x -> x + 2)

def f12(x: (Int, Int)): Unit = ()

@test
def testSpawn12(): Unit = spawn f12((2, 3))

def f13(x: (Int, Int), y: (Int, Int)): Unit = ()

@test
def testSpawn13(): Unit = spawn f13((2, 3), (3, 4))

def f14(x: Channel[Int]): Unit = ()

@test
def testSpawn14(): Unit = spawn f14(channel Int)

def f16(): Int = 42

@test
def testSpawn16(): Unit = spawn f16()

def f17(): Str = "str"

@test
def testSpawn17(): Unit = spawn f17()

def f18(): (Int, Int) = (10, 42)

@test
def testSpawn18(): Unit = spawn f18()

def f19(): Color = Red

@test
def testSpawn19(): Unit = spawn f19()

def f20(): Color = Color.New("Green")

@test
def testSpawn20(): Unit = spawn f20()

def f21(x: Int): Int = x

@test
def testSpawn21(): Unit = spawn f21(3)

def f22(x: Str): Str = x

@test
def testSpawn22(): Unit = spawn f22("str")

def f23(x: Char): Char = x

@test
def testSpawn23(): Unit = spawn f23('2')

def f24(x: Float64): Float64 = x

@test
def testSpawn24(): Unit = spawn f24(3.2)

def f25(x: Int, y: Int): Int = x + y

@test
def testSpawn25(): Unit = spawn f25(1, 2)

def f26(x: Int, y: Str): Int = x

@test
def testSpawn26(): Unit = spawn f26(1, "str")

def f27(x: Int, y: Char): Int = x

@test
def testSpawn27(): Unit = spawn f27(1, 'c')

def f28(x: Int, y: Float64): Int = x

@test
def testSpawn28(): Unit = spawn f28(1, 2.2)

def f29(x: Int, y: Int, z: Int): Int = x + y + z

@test
def testSpawn29(): Unit = spawn f29(1, 2, 3)

def f30(x: Unit): Unit = ()

@test
def testSpawn30(): Unit = spawn f30(spawn f30(()))
