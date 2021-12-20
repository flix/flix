/// Construct colorful messages.
def main(_args: Array[String]): Int32 & Impure =
  let s1 = Console.red("This is a message in red text.");
  println(s1);

  let s2 = Console.bgBlue("This message has a blue background.");
  println(s2);

  let s3 = Console.bgYellow(Console.magenta("This message has both magenta text and yellow background."));
  println(s3);

  let s4 = Console.black("This is ") :: Console.red("c") :: Console.green("o") ::
            Console.yellow("l") :: Console.blue("o") :: Console.magenta("r") ::
            Console.cyan("f") :: Console.greenBright("u") :: Console.blueBright("l") :: Nil;
  let s5 = List.map(s -> Console.bgWhite(s), s4);
  List.map(s -> print(s), s5);
    0 // exit code
