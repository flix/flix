def implicitlyPureWithHttp(): Int32 =
    let x = 123;
    let y = 456;
    let _ = Net.Http.get("http://example.com/");
    x + y
