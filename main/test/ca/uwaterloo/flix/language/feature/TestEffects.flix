///
/// Declares a simple effect.
///
eff f(): Bool

///
/// Declares a handler for the effect `f`.
///
handler f(): Bool = true



///
/// Declares an effect with one argument.
///
eff g(x: Int): Bool

///
/// Declares a handler for the effect `g`.
///
handler g(x: Int): Bool = x == 42



///
/// Declares an effect with two arguments.
///
eff h(x: Int, y: Int): Bool

///
/// Declares a handler for the effect `h`.
///
handler h(x: Int, y: Int): Bool = x == y


///
/// Declares an effect with one argument.
///
eff i(x: Int): Bool

///
/// Declares a complex handler which takes the continuation.
///
handler i[a](x: Int, k: Bool -> a): a = k(x == 42)
