def f: Bool =
    let g = (x, y) -> (x + " World!");
    assertEq!(g("Hello"), "Hello World!")