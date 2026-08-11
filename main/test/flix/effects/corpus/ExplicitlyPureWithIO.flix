def explicitlyPureWithIO(): Int32 \ { } =
    let x = 123;
    let y = 456;
    println(x + y);
    x + y
