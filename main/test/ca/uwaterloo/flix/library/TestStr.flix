/////////////////////////////////////////////////////////////////////////////
// concat                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def concat01(): Bool = assertEq!("Hello World!", "Hello World!")

@test
def concat02(): Bool = assertEq!("Hello" + " World!", "Hello World!")

@test
def concat03(): Bool = assertEq!("Hello" + " World!", "Hello" + " World!")

@test
def concat04(): Bool = assertEq!("Hello World!" + "", "Hello World!")

@test
def concat05(): Bool = assertEq!("Hello" + " " + "World" + "!", "Hello World!")

@test
def concat06(): Bool =
    let f = x -> (x + " World!");
    assertEq!(f("Hello"), "Hello World!")

@test
def concat07(): Bool =
    let f = (x, y) -> (x + y);
    assertEq!(f("Hello", " World!"), "Hello World!")

@test
def concat08(): Bool =
    let f = flip((x, y) -> (x + y));
    assertEq!(f(" World!", "Hello"), "Hello World!")

@test
def trim01(): Bool = "Hello World" == String.trim("Hello World")

@test
def trim02(): Bool = "abc" == String.trim(" abc")

@test
def trim03(): Bool = "abc" == String.trim("abc ")

@test
def trim04(): Bool = "abc" == String.trim(" abc ")

@test
def trim05(): Bool = "abc" == String.trim("  abc")

@test
def trim06(): Bool = "" == String.trim("  ")


/////////////////////////////////////////////////////////////////////////////
// lineSeparator                                                           //
/////////////////////////////////////////////////////////////////////////////
@test
def lineSeparator01(): Bool =
    assertNot! ("" == String.lineSeparator())
