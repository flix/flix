/// Construct colorful messages.
def main(): Unit & Impure =
  let s1 = "You can print message with " + Console.red("colored text");
  let s2 = " or " + Console.bgBlue("background") + ".";
  println(s1+s2);

  let s3 = Console.bgYellow(Console.magenta("This message has both magenta text and yellow background."));
  println(s3);

  let s4 = Console.black("This is a ") :: Console.red("c") :: Console.green("o") ::
            Console.yellow("l") :: Console.blue("o") :: Console.magenta("r") ::
            Console.cyan("f") :: Console.greenBright("u") :: Console.blueBright("l") ::
            Console.black(" message.") :: Nil;
  let s5 = List.map(s -> Console.bgWhite(s), s4);
  List.foreach(s -> print(s), s5);
  println("");

  let s6 = Console.bold("This message is bold.");
  let s7 = Console.hex("#b891eb", " And this is a custom hex color.");
  println(s6 + s7)
