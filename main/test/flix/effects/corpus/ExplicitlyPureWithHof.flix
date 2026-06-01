def explicitlyPureWithHof(f: Unit -> Int32 \ ef): Int32 \ { } =
    let x = 123;
    let y = 456;
    let _ = f();
    x + y
