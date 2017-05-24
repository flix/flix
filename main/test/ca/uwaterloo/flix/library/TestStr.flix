/////////////////////////////////////////////////////////////////////////////
// concat                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def concat01: Bool = assertEq!("Hello World!", "Hello World!")

@test
def concat02: Bool = assertEq!("Hello" + " World!", "Hello World!")

@test
def concat03: Bool = assertEq!("Hello" + " World!", "Hello" + " World!")

@test
def concat04: Bool = assertEq!("Hello World!" + "", "Hello World!")

@test
def concat05: Bool = assertEq!("Hello" + " " + "World" + "!", "Hello World!")

@test
def concat06: Bool =
    let f = x -> (x + " World!");
    assertEq!(f("Hello"), "Hello World!")

@test
def concat07: Bool =
    let f = (x, y) -> (x + y);
    assertEq!(f("Hello", " World!"), "Hello World!")

@test
def concat08: Bool =
    let f = flip((x, y) -> (x + y));
    assertEq!(f(" World!", "Hello"), "Hello World!")
