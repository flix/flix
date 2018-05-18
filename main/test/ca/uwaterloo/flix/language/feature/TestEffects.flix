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
/// Declares a default handler for the effect `i`.
///
handler i[a](x: Bool, y: Bool, k: (Int, Int) -> a): a = if (x && y) k(1, 2) else k(2, 3)

//
/////
///// Effectfully returns `true` if the given path `p` is readable.
/////
//pub eff isReadable[a](p: Path, k: Result[Bool, IOError] -> a): a
//
/////
///// Effectfully returns `true` if the given path `p` is writable.
/////
//pub eff isWritable[a](p: Path, k: Result[Bool, IOError] -> a): a
//
/////
///// Default handler for `isReadable`.
/////
//handler isReadable[a](p: Path, k: Result[Bool, IOError] -> a): a = unsafe {
//    let Path(nativePath) = p;
//    let result = native method java.nio.file.Files.isReadable(nativePath);
//        k(Ok(result))
//}
//
/////
///// Default handler for `isWritable`.
/////
//handler isWritable[a](p: Path, k: Result[Bool, IOError] -> a): a = unsafe {
//   let Path(nativePath) = p;
//   let result = native method java.nio.file.Files.isWritable(nativePath);
//       k(Ok(result))
//}
//
//eff default[a, b](x: a): b
//
//handler default[a, b](x: a): b = x
//
//def pureMain[a](): a =
//    let p = Path.new("foo.flix");
//        isReadable(p, ra ->
//            isWritable(p,
//                rw -> match (ra, rw) with {
//                    case (Ok(true), Ok(true)) => default("Yes, the file is readable and writable.")
//                    case _                    => default("No,  the file is not readable and writable.")
//                }
//            ))
//
//def main1(): Str = pureMain()
//
//def main2(): Str = do
//        pureMain()
//    with {
//        eff default = (x: Str) -> "hello world",
//        eff isReadable = (p: Path, k: Result[Bool, IOError] -> Str) -> k(Ok(true)) : Str,
//        eff isWritable = (p: Path, k: Result[Bool, IOError] -> Str) -> k(Ok(true)) : Str
//    }
//
//def main3(): Str = do
//        pureMain()
//    with {
//        eff default = (x: Str) -> x,
//        eff isReadable = (p: Path, k: Result[Bool, IOError] -> Str) -> k(Ok(true)) : Str,
//        eff isWritable = (p: Path, k: Result[Bool, IOError] -> Str) -> k(Ok(false)) : Str
//    }
//
//def main(): List[Str] = do
//        pureMain() : List[Str]
//    with {
//        eff default = (x: Str) -> (x :: Nil) : List[Str],
//        eff isReadable = (p: Path, k: Result[Bool, IOError] -> List[Str]) -> k(Ok(true)) : List[Str],
//        eff isWritable = (p: Path, k: Result[Bool, IOError] -> List[Str]) -> k(Ok(true)) ::: k(Ok(true)) : List[Str]
//    }
//

