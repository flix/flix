/////////////////////////////////////////////////////////////////////////////
// Native Constructor                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def nativeNew01(): Bool @ Any = assertEq!(unsafe native new java.lang.String(), "")

@test
def nativeNew02(): Bool @ Any =
    let x = unsafe native new java.lang.Character('c');
    let y = unsafe native new java.lang.Character('c');
    unsafe native method java.lang.Character.equals(x, y)

@test
def nativeNew03(): Bool @ Any =
    let x = unsafe native new java.util.BitSet();
    assertEq!(unsafe native method java.util.BitSet.toString(x), "{}")

/////////////////////////////////////////////////////////////////////////////
// Native Static Field                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def nativeField01(): Bool @ Any = assertEq!(unsafe native field java.lang.Character.BYTES, 2)

@test
def nativeField02(): Bool @ Any = assertEq!(unsafe native field java.lang.Character.SIZE, 16)

@test
def nativeField03(): Bool @ Any = assertEq!(unsafe native field java.lang.Long.SIZE, 64)

@test
def nativeField04(): Bool @ Any = assertEq!(unsafe native field java.lang.Float.SIZE, 32)

/////////////////////////////////////////////////////////////////////////////
// Native Static Methods                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def nativeMethod01(): Bool @ Any = assertEq!(unsafe native method java.lang.Integer.bitCount(2), 1)

@test
def nativeMethod02(): Bool @ Any = assertEq!(unsafe native method java.lang.Integer.bitCount(7), 3)

@test
def nativeMethod03(): Bool @ Any = !(unsafe native method java.lang.Boolean.logicalAnd(true, false))

@test
def nativeMethod04(): Bool @ Any = unsafe native method java.lang.Boolean.logicalOr(true, false)

@test
def nativeMethod05(): Bool @ Any = !(unsafe native method java.lang.Boolean.logicalOr(false, false))

@test
def nativeMethod06(): Bool @ Any =
    let x = unsafe native method java.lang.Byte.decode("8");
    assertEq!(unsafe native method java.lang.Byte.intValue(x), 8)

/////////////////////////////////////////////////////////////////////////////
// Native Static Methods                                                   //
/////////////////////////////////////////////////////////////////////////////
@test
def nativeMethod07(): Bool @ Any =
    let x = unsafe native new java.lang.String();
    unsafe native method java.lang.String.isEmpty(x)

@test
def nativeMethod08(): Bool @ Any =
    let x = unsafe native new java.lang.String();
    assertEq!(unsafe native method java.lang.String.length(x), 0)

@test
def nativeMethod09(): Bool @ Any =
    let x = unsafe native new java.util.Date(96, 6, 12);
    let y = unsafe native new java.util.Date(100, 0, 0);
    unsafe native method java.util.Date.before(x, y)

@test
def nativeMethod10(): Bool @ Any =
    let x = unsafe native new java.util.Date(100, 0, 0);
    let y = unsafe native new java.util.Date(96, 6, 12);
    !(unsafe native method java.util.Date.before(x, y))

@test
def nativeMethod11(): Bool @ Any =
    let x = unsafe native new java.util.Date(96, 6, 12);
    let y = unsafe native new java.util.Date(96, 6, 12);
    unsafe native method java.util.Date.equals(x, y)

@test
def nativeMethod12(): Bool @ Any =
    let x = unsafe native new java.util.Date(96, 6, 12);
    let y = unsafe native new java.util.Date(100, 0, 0);
    !(unsafe native method java.util.Date.equals(x, y))

@test
def nativeMethod13(): Bool @ Any =
    let x = unsafe native new java.util.BitSet();
    let y = unsafe native method java.util.BitSet.set(x, 10);
    let z = unsafe native method java.util.BitSet.set(x, 3);
    assertEq!(unsafe native method java.util.BitSet.toString(x), "{3, 10}")
