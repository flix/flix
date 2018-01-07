///
/// Declares an effect.
///
eff f[a](k: Bool -> a): a

///
/// Declares a default handler for the effect `f`.
///
handler f[a](k: Bool -> a): a = k(true)

///
/// Declares an effect.
///
eff g[a](x: Int, k: Bool -> a): a

///
/// Declares a default handler for the effect `g`.
///
handler g[a](x: Int, k: Bool -> a): a = if (x == 0) k(true) else k(false)

///
/// Declares an effect.
///
eff h[a](x: Int, y: Int, k: Bool -> a): a

///
/// Declares a default handler for the effect `h`.
///
handler h[a](x: Int, y: Int, k: Bool -> a): a = if (x == y) k(true) else k(false)

///
/// Declares an effect.
///
eff i[a](x: Bool, y: Bool, k: (Int, Int) -> a): a

///
/// Declares a default handler for the effect `h`.
///
handler i[a](x: Bool, y: Bool, k: (Int, Int) -> a): a = if (x && y) k((1, 2)) else k((2, 3))

