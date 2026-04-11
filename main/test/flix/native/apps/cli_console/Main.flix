///
/// A small "real program" fixture for the LLVM-native backend.
///
/// Exercises:
/// - `Env.getArgs` (argv plumbing)
/// - `Console.readln/println` (stdio plumbing)
/// - basic parsing + arithmetic
///
def main(): Unit \ IO =
    run {
        let args = Env.getArgs();
        let prefix = match args {
            case x :: _ => x
            case Nil => "sum"
        };

        Console.println("cli-console: ready");
        Console.println("cli-console: enter a");
        let aLine = Console.readln();
        Console.println("cli-console: enter b");
        let bLine = Console.readln();

        let a = match Int32.fromString(aLine) {
            case Some(x) => x
            case None => bug!("cli-console: bad Int32: " + aLine)
        };
        let b = match Int32.fromString(bLine) {
            case Some(x) => x
            case None => bug!("cli-console: bad Int32: " + bLine)
        };

        Console.println(prefix + ":" + Int32.toString(a + b));
        ()
    } with Env.runWithIO
      with Console.runWithIO
